{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StrictData #-}

module Duffer.Loose.Objects where

import qualified Data.ByteString           as B
import qualified Data.ByteString.Base16    as B16 (decode)
import qualified Data.ByteString.Base64    as B64
import qualified ByteString.TreeBuilder    as TB
import qualified Data.ByteString.UTF8      as UB (toString)
import qualified Data.Set                  as S
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as E

import Control.Applicative     (empty)
import Crypto.Hash             (hashWith)
import Crypto.Hash.Algorithms  (SHA1)
import Data.Aeson              (ToJSON(..), FromJSON(..), KeyValue, Value(..)
                               ,object, pairs, (.=), (.:), (.:?), withObject)
import Data.Bool               (bool)
import Data.ByteArray.Encoding (Base(Base16), convertToBase)
import Data.Byteable           (Byteable(toBytes))
import Data.ByteString.UTF8    (fromString, toString)
import Data.Function           (on)
import Data.List               (intercalate)
import Data.Maybe              (fromMaybe)
import Data.Monoid             ((<>))
import Data.Set                (Set, toAscList)
import Data.String             (IsString)
import Numeric                 (readOct)
import System.FilePath         ((</>))
import Text.Printf             (printf)

data GitObjectGeneric ref entries
    = Blob {blobContent :: B.ByteString}
    | Tree {treeEntries :: entries}
    | Commit
        { commitTreeRef    :: ref
        , commitParentRefs :: [ref]
        , commitAuthor     :: PersonTime
        , commitCommitter  :: PersonTime
        , commitSignature  :: Maybe B.ByteString
        , commitMessage    :: B.ByteString
        }
    | Tag
        { tagObjectRef  :: ref
        , tagObjectType :: B.ByteString
        , tagName       :: B.ByteString
        , tagTagger     :: PersonTime
        , tagAnnotation :: B.ByteString
        }
    deriving (Eq)

type GitObject = GitObjectGeneric Ref (Set TreeEntry)

-- A tree entry has permissions, a file/directory name, and a ref.
data TreeEntry = TreeEntry
    { entryPerms :: EntryPermission
    , entryName  :: B.ByteString
    , entryRef   :: Ref
    }

data PersonTime = PersonTime
    { personName :: B.ByteString
    , personMail :: B.ByteString
    , personTime :: B.ByteString
    , personTZ   :: B.ByteString
    }
    deriving (Eq)

data EntryPermission
    = Directory
    | Regular
    | Executable
    | SymbolicLink
    | SubModule
    deriving (Show, Eq)

type Ref  = B.ByteString
type Repo = FilePath

instance Show GitObject where
    show (Tree entries) = unlines . map show $ toAscList entries
    show other          = UB.toString . TB.toByteString $ showContent other

instance Byteable PersonTime where
    toBytes (PersonTime name mail time zone) =
        B.concat [name, " <", mail, "> ", time, " ", zone]

instance Show PersonTime where show = toString . toBytes

instance Show TreeEntry where
    show (TreeEntry mode name sha1) = intercalate "\t" components
        where components = [octMode, entryType, toString sha1, toString name]
              octMode    = printf "%06o" (fromEnum mode) :: String
              entryType  = case mode of
                Directory -> "tree"
                SubModule -> "commit"
                _         -> "blob"

instance Eq  TreeEntry where (==)    = (==)    `on` sortableName
instance Ord TreeEntry where compare = compare `on` sortableName

sortableName :: TreeEntry -> B.ByteString
sortableName TreeEntry{..} = entryName `B.append`
    bool "" "/" (entryPerms == Directory || entryPerms == SubModule)

instance Byteable TreeEntry where
    toBytes (TreeEntry mode name sha1) = let
        mode' = fromString $ printf "%o" (fromEnum mode)
        sha1' = fst $ B16.decode sha1
        in B.concat [mode', " ", name, "\NUL", sha1']

instance Byteable GitObject where toBytes = showObject

instance Enum EntryPermission where
    fromEnum p = case p of
        Directory    -> 0o040000
        Regular      -> 0o100644
        Executable   -> 0o100755
        SymbolicLink -> 0o120000
        SubModule    -> 0o160000
    toEnum i = case i of
        0o040000 -> Directory
        0o100644 -> Regular
        0o100755 -> Executable
        0o120000 -> SymbolicLink
        0o160000 -> SubModule
        _        -> error "invalid permission"

sha1Path :: Ref -> Repo -> FilePath
sha1Path ref = let (sa:sb:suffix) = toString ref in
    flip (foldl (</>)) [[sa, sb], suffix]

-- Generate a stored representation of a git object.
showObject :: GitObject -> B.ByteString
showObject gitObject = header `B.append` content
    where content    = TB.toByteString $ showContent gitObject
          header     = B.concat [objectType gitObject, " ", len, "\NUL"]
          len        = fromString . show $ B.length content

objectType :: IsString a => GitObjectGeneric r e -> a
objectType someObject = case someObject of
    Blob{}   -> "blob"
    Tree{}   -> "tree"
    Commit{} -> "commit"
    Tag{}    -> "tag"

showContent :: GitObject -> TB.Builder
showContent gitObject = case gitObject of
    Blob content -> TB.byteString content
    Tree entries -> mconcat . map (TB.byteString . toBytes) $ toAscList entries
    Commit {..}  -> mconcat
        [                "tree"      ?                commitTreeRef
        , mconcat $ map ("parent"    ?)               commitParentRefs
        ,                "author"    ?  toBytes       commitAuthor
        ,                "committer" ?  toBytes       commitCommitter
        , maybe mempty  ("gpgsig"    ?)               commitSignature
        ,                "\n"        ,  TB.byteString commitMessage
        ]
    Tag {..} -> mconcat
        [ "object" ?               tagObjectRef
        , "type"   ?               tagObjectType
        , "tag"    ?               tagName
        , "tagger" ?       toBytes tagTagger
        , "\n"     , TB.byteString tagAnnotation
        ]
    where (?) key value = mconcat $ map TB.byteString [key, " ", value, "\n"]

hashSHA1 :: B.ByteString -> Ref
hashSHA1 = convertToBase Base16 . hashWith (undefined :: SHA1)

hash :: GitObject -> Ref
hash = hashSHA1 . toBytes

b64encode :: B.ByteString -> T.Text
b64encode = E.decodeUtf8 . B64.encode

b64decode :: T.Text -> B.ByteString
b64decode = B64.decodeLenient . E.encodeUtf8

decodeRef :: Ref -> T.Text
decodeRef = E.decodeUtf8

decodeBS :: B.ByteString -> T.Text
decodeBS  = E.decodeUtf8

encodeRef :: T.Text -> Ref
encodeRef = E.encodeUtf8

encodeBS :: T.Text -> B.ByteString
encodeBS  = E.encodeUtf8

gitObjectPairs :: KeyValue t => GitObject -> [t]
gitObjectPairs obj = ["object_type" .= String (objectType obj)] <> case obj of
    Blob {..} ->
        [ "content"    .= b64encode blobContent ]
    Tree {..} ->
        [ "entries"    .= S.toList treeEntries ]
    Commit {..} ->
        [ "tree"       .=     decodeRef commitTreeRef
        , "parents"    .= map decodeRef commitParentRefs
        , "author"     .=               commitAuthor
        , "committer"  .=               commitCommitter
        , "gpgsig"     .= may decodeBS  commitSignature
        , "message"    .=     decodeBS  commitMessage
        ]
    Tag {..} ->
        [ "object"     .= decodeRef tagObjectRef
        , "type"       .= decodeBS  tagObjectType
        , "name"       .= decodeBS  tagName
        , "tagger"     .=           tagTagger
        , "annotation" .= decodeBS  tagAnnotation
        ]
    where may decoder value = fromMaybe Null (String . decoder <$> value)

treeEntryPairs :: KeyValue t => TreeEntry -> [t]
treeEntryPairs TreeEntry {..} =
    ["mode" .= octMode, "name" .= dName, "ref" .= dRef]
    where octMode = T.pack $ printf "%06o" (fromEnum entryPerms) :: T.Text
          dName   = E.decodeUtf8 entryName
          dRef    = decodeRef entryRef

personTimePairs :: KeyValue t => PersonTime -> [t]
personTimePairs PersonTime {..} =
    [ "name" .= decodeBS personName
    , "mail" .= decodeBS personMail
    , "time" .= decodeBS personTime
    , "zone" .= decodeBS personTZ
    ]

instance ToJSON GitObject where
    toJSON     = object . gitObjectPairs
    toEncoding = pairs  . foldr1 (<>) . gitObjectPairs

instance ToJSON TreeEntry where
    toJSON     = object . treeEntryPairs
    toEncoding = pairs  . foldr1 (<>) . treeEntryPairs

instance ToJSON PersonTime where
    toJSON     = object . personTimePairs
    toEncoding = pairs  . foldr1 (<>) . personTimePairs

instance FromJSON GitObject where
    parseJSON = withObject "GitObject" $ \v -> v .: "object_type" >>= \oType ->
        case (oType :: String) of
            "blob"   -> Blob <$> (b64decode  <$> v .: "content")
            "tree"   -> Tree <$> (S.fromList <$> v .: "entries")
            "commit" -> Commit
                <$> (encodeRef      <$> v .:  "tree")
                <*> (map encodeRef  <$> v .:  "parents")
                <*>                     v .:  "author"
                <*>                     v .:  "committer"
                <*> ((encodeBS <$>) <$> v .:? "gpgsig")
                <*> (encodeBS       <$> v .:  "message")
            "tag" -> Tag
                <$> (encodeRef <$> v .: "object")
                <*> (encodeBS  <$> v .: "type")
                <*> (encodeBS  <$> v .: "name")
                <*>                v .: "tagger"
                <*> (encodeBS  <$> v .: "annotation")
            _ -> empty

instance FromJSON TreeEntry where
    parseJSON = withObject "TreeEntry" $ \v -> TreeEntry
        <$> (readOctal    <$> v .: "mode")
        <*> (E.encodeUtf8 <$> v .: "name")
        <*> (E.encodeUtf8 <$> v .: "ref")
        where readOctal = toEnum . fst . head . readOct . T.unpack

instance FromJSON PersonTime where
    parseJSON = withObject "PersonTime" $ \v ->
        let enc = fmap E.encodeUtf8 . (.:) v in PersonTime
        <$> enc "name" <*> enc "mail" <*> enc "time" <*> enc "zone"
