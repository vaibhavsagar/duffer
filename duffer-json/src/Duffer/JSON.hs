{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}

module Duffer.JSON
    ( RefJSON(..)
    , GitObjectJSON(..)
    , TreeEntryJSON(..)
    , PersonTimeJSON(..)
    ) where

import qualified Data.ByteString        as B
import qualified Data.ByteString.Base64 as B64
import qualified Data.Set               as S
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as E

import Data.Aeson              (ToJSON(..), FromJSON(..), KeyValue, Value(..)
                               ,object, pairs, (.=), (.:), (.:?), withObject)
import Data.Coerce             (coerce)
import Data.Monoid             ((<>), mconcat)
import Numeric                 (readOct)
import Text.Printf             (printf)

import Duffer.Loose.Objects (Ref, GitObjectGeneric(..), GitObject, TreeEntry(..)
                            ,PersonTime(..), Commit(..), Tree(..), Blob(..)
                            ,Tag(..), objectType)

newtype RefJSON        = RefJSON        Ref
newtype GitObjectJSON  = GitObjectJSON  GitObject
newtype TreeEntryJSON  = TreeEntryJSON  TreeEntry deriving (Eq, Ord)
newtype PersonTimeJSON = PersonTimeJSON PersonTime

b64encode :: B.ByteString -> T.Text
b64encode = B64.encodeBase64

b64decode :: T.Text -> B.ByteString
b64decode = B64.decodeBase64Lenient . E.encodeUtf8

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
    GitBlob Blob{..} ->
        [ "content"    .= b64encode blobContent ]
    GitTree Tree{..} ->
        [ "entries"    .= S.map TreeEntryJSON treeEntries ]
    GitCommit Commit{..} ->
        [ "tree"       .=     decodeRef  commitTreeRef
        , "parents"    .= map decodeRef  commitParentRefs
        , "author"     .= PersonTimeJSON commitAuthor
        , "committer"  .= PersonTimeJSON commitCommitter
        , "gpgsig"     .= may decodeBS   commitSignature
        , "message"    .=     decodeBS   commitMessage
        ]
    GitTag Tag{..} ->
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

instance ToJSON RefJSON where
    toJSON     (RefJSON ref) = object ["ref" .= decodeRef ref]
    toEncoding (RefJSON ref) = pairs  ("ref" .= decodeRef ref)

instance FromJSON RefJSON where
    parseJSON = withObject "Ref" $ \v -> coerce <$> (encodeRef <$> v .: "ref")

instance ToJSON GitObjectJSON where
    toJSON     = object . gitObjectPairs . coerce
    toEncoding = pairs  . mconcat . gitObjectPairs . coerce

instance ToJSON TreeEntryJSON where
    toJSON     = object . treeEntryPairs . coerce
    toEncoding = pairs  . mconcat . treeEntryPairs . coerce

instance ToJSON PersonTimeJSON where
    toJSON     = object . personTimePairs . coerce
    toEncoding = pairs  . mconcat . personTimePairs . coerce

instance FromJSON GitObjectJSON where
    parseJSON = withObject "GitObject" $ \v -> v .: "object_type" >>= \t ->
        coerce <$> case (t :: String) of
            "blob"   -> GitBlob . Blob <$> (b64decode      <$> v .: "content")
            "tree"   -> GitTree . Tree <$> (S.map encodeTE <$> v .: "entries")
            "commit" -> fmap GitCommit $ Commit
                <$> (     encodeRef <$> v .:  "tree")
                <*> (map  encodeRef <$> v .:  "parents")
                <*> (     encodePT  <$> v .:  "author")
                <*> (     encodePT  <$> v .:  "committer")
                <*> (fmap encodeBS  <$> v .:? "gpgsig")
                <*> (     encodeBS  <$> v .:  "message")
            "tag"    -> fmap GitTag $ Tag
                <$> (encodeRef <$> v .: "object")
                <*> (encodeBS  <$> v .: "type")
                <*> (encodeBS  <$> v .: "name")
                <*> (encodePT  <$> v .: "tagger")
                <*> (encodeBS  <$> v .: "annotation")
            _        -> fail $ "unknown object_type: " <> t
        where
            encodePT = coerce @PersonTimeJSON
            encodeTE = coerce @TreeEntryJSON

instance FromJSON TreeEntryJSON where
    parseJSON = withObject "TreeEntry" $ \v -> fmap coerce $ TreeEntry
        <$> (readOctal    <$> v .: "mode")
        <*> (E.encodeUtf8 <$> v .: "name")
        <*> (E.encodeUtf8 <$> v .: "ref")
        where readOctal = toEnum . fst . head . readOct . T.unpack

instance FromJSON PersonTimeJSON where
    parseJSON = withObject "PersonTime" $ \v ->
        let enc = fmap E.encodeUtf8 . (.:) v in fmap coerce $ PersonTime
        <$> enc "name" <*> enc "mail" <*> enc "time" <*> enc "zone"
