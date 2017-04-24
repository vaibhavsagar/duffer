{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StrictData #-}

module Duffer.JSON where

import qualified Data.ByteString        as B
import qualified Data.ByteString.Base64 as B64
import qualified Data.Set               as S
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as E

import Data.Aeson              (ToJSON(..), FromJSON(..), KeyValue, Value(..)
                               ,object, pairs, (.=), (.:), (.:?), withObject)
import Data.Monoid             ((<>), mconcat)
import Numeric                 (readOct)
import Text.Printf             (printf)

import Duffer.Loose.Objects (Ref, GitObjectGeneric(..), GitObject, TreeEntry(..), PersonTime(..), objectType)

newtype GitObjectJSON  = GitObjectJSON  { innerObject :: GitObject  }
newtype TreeEntryJSON  = TreeEntryJSON  { innerEntry  :: TreeEntry  } deriving (Eq, Ord)
newtype PersonTimeJSON = PersonTimeJSON { innerPT     :: PersonTime }

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
        [ "entries"    .= S.map TreeEntryJSON treeEntries ]
    Commit {..} ->
        [ "tree"       .=     decodeRef  commitTreeRef
        , "parents"    .= map decodeRef  commitParentRefs
        , "author"     .= PersonTimeJSON commitAuthor
        , "committer"  .= PersonTimeJSON commitCommitter
        , "gpgsig"     .= may decodeBS   commitSignature
        , "message"    .=     decodeBS   commitMessage
        ]
    Tag {..} ->
        [ "object"     .= decodeRef      tagObjectRef
        , "type"       .= decodeBS       tagObjectType
        , "name"       .= decodeBS       tagName
        , "tagger"     .= PersonTimeJSON tagTagger
        , "annotation" .= decodeBS       tagAnnotation
        ]
    where may = maybe Null . (.) String

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

instance ToJSON GitObjectJSON where
    toJSON     = object . gitObjectPairs . innerObject
    toEncoding = pairs  . mconcat . gitObjectPairs . innerObject

instance ToJSON TreeEntryJSON where
    toJSON     = object . treeEntryPairs . innerEntry
    toEncoding = pairs  . mconcat . treeEntryPairs . innerEntry

instance ToJSON PersonTimeJSON where
    toJSON     = object . personTimePairs . innerPT
    toEncoding = pairs  . mconcat . personTimePairs . innerPT

instance FromJSON GitObjectJSON where
    parseJSON = withObject "GitObject" $ \v -> v .: "object_type" >>= \t ->
        GitObjectJSON <$> case (t :: String) of
            "blob"   -> Blob <$> (b64decode        <$> v .: "content")
            "tree"   -> Tree <$> (S.map innerEntry <$> v .: "entries")
            "commit" -> Commit
                <$> (    encodeRef <$> v .:  "tree")
                <*> (map encodeRef <$> v .:  "parents")
                <*> fmap innerPT      (v .:  "author")
                <*> fmap innerPT      (v .:  "committer")
                <*> (fmap encodeBS <$> v .:? "gpgsig")
                <*> (     encodeBS <$> v .:  "message")
            "tag"    -> Tag
                <$> (encodeRef <$> v .: "object")
                <*> (encodeBS  <$> v .: "type")
                <*> (encodeBS  <$> v .: "name")
                <*> (innerPT   <$> v .: "tagger")
                <*> (encodeBS  <$> v .: "annotation")
            _        -> fail $ "unknown object_type: " <> t

instance FromJSON TreeEntryJSON where
    parseJSON = withObject "TreeEntry" $ \v -> fmap TreeEntryJSON $ TreeEntry
        <$> (readOctal    <$> v .: "mode")
        <*> (E.encodeUtf8 <$> v .: "name")
        <*> (E.encodeUtf8 <$> v .: "ref")
        where readOctal = toEnum . fst . head . readOct . T.unpack

instance FromJSON PersonTimeJSON where
    parseJSON = withObject "PersonTime" $ \v ->
        let enc = fmap E.encodeUtf8 . (.:) v in fmap PersonTimeJSON $ PersonTime
        <$> enc "name" <*> enc "mail" <*> enc "time" <*> enc "zone"
