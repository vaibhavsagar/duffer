{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Duffer.Loose.Objects where

import qualified Data.ByteString           as B
import qualified Data.ByteString.Base16    as B16 (decode)
import qualified Data.ByteString.Base64    as B64
import qualified Data.ByteString.Builder   as BB
import qualified Data.ByteString.Lazy      as L
import qualified Data.ByteString.Lazy.UTF8 as UL (toString)
import qualified Data.HashMap.Strict       as H
import qualified Data.Set                  as S
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as E

import Control.Applicative     (empty)
import Crypto.Hash             (hashWith)
import Crypto.Hash.Algorithms  (SHA1)
import Data.Aeson              (ToJSON(..), FromJSON(..), KeyValue, Value(..)
                               ,object, pairs, (.=), (.:))
import Data.Bool               (bool)
import Data.ByteArray.Encoding (Base(Base16), convertToBase)
import Data.Byteable           (Byteable(..))
import Data.ByteString.UTF8    (fromString, toString)
import Data.Function           (on)
import Data.List               (intercalate)
import Data.Monoid             ((<>))
import Data.Set                (Set, toAscList)
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
    show (Tree entries) = unlines $ map show $ toAscList entries
    show other          = UL.toString $ BB.toLazyByteString $ showContent other

instance Byteable PersonTime where
    toBytes (PersonTime name mail time zone) =
        B.concat [name, " <", mail, "> ", time, " ", zone]

instance Show PersonTime where
    show = toString . toBytes

instance Show TreeEntry where
    show (TreeEntry mode name sha1) = intercalate "\t" components
        where components = [octMode, entryType, toString sha1, toString name]
              octMode    = printf "%06o" (fromEnum mode) :: String
              entryType  = case mode of
                Directory -> "tree"
                SubModule -> "commit"
                _         -> "blob"

instance Eq TreeEntry where
    (==) = (==) `on` entryName

instance Ord TreeEntry where
    compare = compare `on` sortableName
        where sortableName (TreeEntry mode name _) = name `B.append`
                bool "" "/" (mode == Directory || mode == SubModule)

instance Byteable TreeEntry where
    toBytes (TreeEntry mode name sha1) = let
        mode' = fromString $ printf "%o" (fromEnum mode)
        sha1' = fst $ B16.decode sha1
        in B.concat [mode', " ", name, "\NUL", sha1']

instance Byteable GitObject where
    toBytes = L.toStrict . showObject

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
showObject :: GitObject -> L.ByteString
showObject gitObject = header `L.append` content
    where content    = BB.toLazyByteString $ showContent gitObject
          header     = L.concat [objectType, " ", len, "\NUL"]
          objectType = case gitObject of
            Blob{}   -> "blob"
            Tree{}   -> "tree"
            Commit{} -> "commit"
            Tag{}    -> "tag"
          len        = L.fromStrict $ fromString . show $ L.length content

showContent :: GitObject -> BB.Builder
showContent gitObject = case gitObject of
    Blob content -> BB.byteString content
    Tree entries -> mconcat $ map (BB.byteString . toBytes) $ toAscList entries
    Commit {..}  -> mconcat
        [                "tree"      ?                 commitTreeRef
        , mconcat $ map ("parent"    ?)                commitParentRefs
        ,                "author"    ?  toBytes        commitAuthor
        ,                "committer" ?  toBytes        commitCommitter
        ,                "\n"        ,  BB.byteString  commitMessage
        ]
    Tag {..} -> mconcat
        [ "object" ?               tagObjectRef
        , "type"   ?               tagObjectType
        , "tag"    ?               tagName
        , "tagger" ?       toBytes tagTagger
        , "\n"     , BB.byteString tagAnnotation
        ]
    where (?) key value = mconcat $ map BB.byteString [key, " ", value, "\n"]

hash :: GitObject -> Ref
hash = convertToBase Base16 . hashWith (undefined :: SHA1) . toBytes

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
gitObjectPairs obj = case obj of
    Blob {..} ->
        [ "object_type" .= String "blob"
        , "content"     .= b64encode blobContent
        ]
    Tree {..} ->
        [ "object_type" .= String "tree"
        , "entries"     .= S.toList treeEntries
        ]
    Commit {..} ->
        [ "object_type" .= String "commit"
        , "tree"        .=     decodeRef commitTreeRef
        , "parents"     .= map decodeRef commitParentRefs
        , "author"      .=               commitAuthor
        , "committer"   .=               commitCommitter
        , "message"     .=     decodeBS  commitMessage
        ]
    Tag {..} ->
        [ "object_type" .= String "tag"
        , "object"      .= decodeRef tagObjectRef
        , "type"        .= decodeBS  tagObjectType
        , "name"        .= decodeBS  tagName
        , "tagger"      .=           tagTagger
        , "annotation"  .= decodeBS  tagAnnotation
        ]

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
    parseJSON (Object v) = case H.lookup "object_type" v of
        Just "blob"   -> Blob <$> (b64decode  <$> v .: "content")
        Just "tree"   -> Tree <$> (S.fromList <$> v .: "entries")
        Just "commit" -> Commit
            <$> (encodeRef     <$> v .: "tree")
            <*> (map encodeRef <$> v .: "parents")
            <*>                    v .: "author"
            <*>                    v .: "committer"
            <*> (encodeBS      <$> v .: "message")
        Just "tag" -> Tag
            <$> (encodeRef <$> v .: "object")
            <*> (encodeBS  <$> v .: "type")
            <*> (encodeBS  <$> v .: "name")
            <*>                v .: "tagger"
            <*> (encodeBS  <$> v .: "annotation")
        _ -> empty
    parseJSON _ = empty

instance FromJSON TreeEntry where
    parseJSON (Object v) = TreeEntry
        <$> (readOctal    <$> v .: "mode")
        <*> (E.encodeUtf8 <$> v .: "name")
        <*> (E.encodeUtf8 <$> v .: "ref")
        where readOctal = toEnum . fst . head . readOct . T.unpack
    parseJSON _ = empty

instance FromJSON PersonTime where
    parseJSON (Object v) = PersonTime
        <$> (E.encodeUtf8 <$> v .: "name")
        <*> (E.encodeUtf8 <$> v .: "mail")
        <*> (E.encodeUtf8 <$> v .: "time")
        <*> (E.encodeUtf8 <$> v .: "zone")
    parseJSON _ = empty
