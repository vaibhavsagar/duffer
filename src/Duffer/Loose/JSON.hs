{-# LANGUAGE RecordWildCards #-}

module Duffer.Loose.JSON where

import qualified Data.ByteString        as B
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.UTF8   as BU
import qualified Data.Set               as S
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as E
import qualified Data.HashMap.Strict    as H

import Data.Aeson
import Data.Monoid ((<>))
import Text.Printf (printf)

import Duffer.Loose.Objects

b64encode :: B.ByteString -> T.Text
b64encode = E.decodeUtf8 . B64.encode

b64decode :: T.Text -> B.ByteString
b64decode = B64.decodeLenient . E.encodeUtf8

decodeRef :: Ref -> T.Text
decodeRef = E.decodeUtf8

decodeBS :: B.ByteString -> T.Text
decodeBS = E.decodeUtf8

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
        [ "objectRef"  .= decodeRef objectRef
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
    [ "personName" .= decodeBS personName
    , "personMail" .= decodeBS personMail
    , "personTime" .= decodeBS personTime
    , "personTZ"   .= decodeBS personTZ
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
