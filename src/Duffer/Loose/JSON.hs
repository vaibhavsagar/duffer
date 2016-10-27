{-# LANGUAGE RecordWildCards #-}

module Duffer.Loose.JSON where

import qualified Data.ByteString        as B
import qualified Data.ByteString.Base64 as B64
import qualified Data.Set               as S
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as E
import qualified Data.HashMap.Strict    as H

import Control.Applicative (empty)
import Data.Aeson
import Data.Monoid ((<>))
import Numeric     (readOct)
import Text.Printf (printf)

import Duffer.Loose.Objects

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
    Blob {..} -> ["type" .= String "blob", "content" .= b64encode content]
    Tree {..} -> ["type" .= String "tree", "entries" .= S.toList entries]
    Commit {..} ->
        [ "type"          .= String "commit"
        , "treeRef"       .= decodeRef treeRef
        , "parentRefs"    .= map decodeRef parentRefs
        , "authorTime"    .= authorTime
        , "committerTime" .= committerTime
        , "message"       .= decodeBS message
        ]
    Tag {..} ->
        [ "type"       .= String "tag"
        , "objectRef"  .= decodeRef objectRef
        , "objectType" .= T.pack objectType
        , "tagName"    .= T.pack tagName
        , "tagger"     .= tagger
        , "annotation" .= decodeBS annotation
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
    parseJSON (Object v) = case H.lookup "type" v of
        Just "blob"   -> Blob <$> (b64decode  <$> v .: "content")
        Just "tree"   -> Tree <$> (S.fromList <$> v .: "entries")
        Just "commit" -> Commit
            <$> (encodeRef     <$> v .: "treeRef")
            <*> (map encodeRef <$> v .: "parentRefs")
            <*>                    v .: "authorTime"
            <*>                    v .: "committerTime"
            <*> (encodeBS      <$> v .: "message")
        Just "tag" -> Tag
            <$> (encodeRef <$> v .: "objectRef")
            <*> (T.unpack  <$> v .: "objectType")
            <*> (T.unpack  <$> v .: "tagName")
            <*>                v .: "tagger"
            <*> (encodeBS  <$> v .: "annotation")
        _          -> empty
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

