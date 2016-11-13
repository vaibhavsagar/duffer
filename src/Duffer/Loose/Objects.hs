{-# LANGUAGE RecordWildCards #-}

module Duffer.Loose.Objects where

import qualified Data.ByteArray.Encoding   as E
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

import Control.Applicative    (empty)
import Crypto.Hash            (hashWith)
import Crypto.Hash.Algorithms (SHA1)
import Data.Aeson
import Data.Bool              (bool)
import Data.Byteable
import Data.ByteString.UTF8   (fromString, toString)
import Data.List              (intercalate)
import Data.Monoid            ((<>))
import Data.Set               (Set, toAscList)
import Numeric                (readOct)
import System.FilePath        ((</>))
import Text.Printf            (printf)

data GitObject
    = Blob {content :: B.ByteString}
    | Tree {entries :: Set TreeEntry}
    | Commit
        { treeRef       :: Ref
        , parentRefs    :: [Ref]
        , authorTime    :: PersonTime
        , committerTime :: PersonTime
        , message       :: B.ByteString
        }
    | Tag
        { objectRef  :: Ref
        , objectType :: B.ByteString
        , tagName    :: B.ByteString
        , tagger     :: PersonTime
        , annotation :: B.ByteString
        }
    deriving (Eq)

-- A tree entry has permissions, a file/directory name, and a ref.
data TreeEntry = TreeEntry
    { entryPerms :: Int
    , entryName  :: B.ByteString
    , entryRef   :: Ref
    }
    deriving (Eq)

data PersonTime = PersonTime
    { personName :: B.ByteString
    , personMail :: B.ByteString
    , personTime :: B.ByteString
    , personTZ   :: B.ByteString
    }
    deriving (Eq)

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
        where components = [octMode, entryType, sha1', toString name]
              octMode    = printf "%06o" mode :: String
              sha1'      = toString sha1
              entryType  = case mode of
                0o040000 -> "tree"
                0o160000 -> "commit"
                _        -> "blob"

instance Ord TreeEntry where
    compare t1 t2 = compare (sortableName t1) (sortableName t2)
        where sortableName (TreeEntry mode name _) = name `B.append`
                bool "" "/" (mode == 0o040000 || mode == 0o160000)

instance Byteable TreeEntry where
    toBytes (TreeEntry mode name sha1) = let
        mode' = fromString $ printf "%o" mode
        sha1' = fst $ B16.decode sha1
        in B.concat [mode', " ", name, "\NUL", sha1']

instance Byteable GitObject where
    toBytes = L.toStrict . showObject

sha1Path :: Ref -> Repo -> FilePath
sha1Path ref = let (sa:sb:suffix) = toString ref in
    flip (foldl (</>)) ["objects", [sa, sb], suffix]

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
        [                "tree"      ?  treeRef
        , mconcat $ map ("parent"    ?) parentRefs
        ,                "author"    ?  toBytes authorTime
        ,                "committer" ?  toBytes committerTime
        ,                "\n"        ,  BB.byteString message
        ]
    Tag {..} -> mconcat
        [ "object" ?               objectRef
        , "type"   ?               objectType
        , "tag"    ?               tagName
        , "tagger" ?       toBytes tagger
        , "\n"     , BB.byteString annotation
        ]
    where (?) key value =
            mconcat $ map BB.byteString [key, " ", value, "\n"]

hash :: GitObject -> Ref
hash = E.convertToBase E.Base16 . hashWith (undefined :: SHA1) . toBytes

b64encode :: B.ByteString -> T.Text
b64encode = E.decodeUtf8 . B64.encode

b64decode :: T.Text -> B.ByteString
b64decode = B64.decodeLenient . E.encodeUtf8

decodeRef, decodeBS :: Ref -> T.Text
decodeRef = E.decodeUtf8
decodeBS  = E.decodeUtf8

encodeRef, encodeBS :: T.Text -> B.ByteString
encodeRef = E.encodeUtf8
encodeBS  = E.encodeUtf8

gitObjectPairs :: KeyValue t => GitObject -> [t]
gitObjectPairs obj = case obj of
    Blob {..} ->
        [ "object_type" .= String "blob"
        , "content"     .= b64encode content
        ]
    Tree {..} ->
        [ "object_type" .= String "tree"
        , "entries"     .= S.toList entries
        ]
    Commit {..} ->
        [ "object_type" .= String "commit"
        , "tree"        .= decodeRef treeRef
        , "parents"     .= map decodeRef parentRefs
        , "author"      .= authorTime
        , "committer"   .= committerTime
        , "message"     .= decodeBS message
        ]
    Tag {..} ->
        [ "object_type" .= String "tag"
        , "object"      .= decodeRef objectRef
        , "type"        .= decodeBS objectType
        , "name"        .= decodeBS tagName
        , "tagger"      .= tagger
        , "annotation"  .= decodeBS annotation
        ]

treeEntryPairs :: KeyValue t => TreeEntry -> [t]
treeEntryPairs TreeEntry {..} =
    ["mode" .= octMode, "name" .= dName, "ref" .= dRef]
    where octMode = T.pack $ printf "%06o" entryPerms :: T.Text
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
        where readOctal = fst . head . readOct . T.unpack
    parseJSON _ = empty

instance FromJSON PersonTime where
    parseJSON (Object v) = PersonTime
        <$> (E.encodeUtf8 <$> v .: "name")
        <*> (E.encodeUtf8 <$> v .: "mail")
        <*> (E.encodeUtf8 <$> v .: "time")
        <*> (E.encodeUtf8 <$> v .: "zone")
    parseJSON _ = empty
