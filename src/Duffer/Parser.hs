module Duffer.Parser where

import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as L

import Data.Attoparsec.ByteString.Char8 hiding (takeTill)
import Prelude                          hiding (take)

import Data.Attoparsec.ByteString
import Data.ByteString.Base16.Lazy     (encode)
import Data.ByteString.UTF8            (toString)
import Data.Set                        (fromList)
import Numeric                         (readOct)

import Duffer.Types

parseNull :: Parser Char
parseNull = char '\NUL'

parseHeader :: B.ByteString -> Parser String
parseHeader = (*> digit `manyTill` parseNull) . (*> space) . string

parseRestOfLine :: Parser String
parseRestOfLine = toString <$> takeTill (==10) <* endOfLine

parseMessage :: Parser B.ByteString
parseMessage = endOfLine *> (B.init <$> takeByteString)

parseRef :: Parser Ref
parseRef = L.fromStrict <$> take 40 <* endOfLine

parseBlob :: Parser GitObject
parseBlob = parseHeader "blob" >> Blob <$> takeByteString

parseTree :: Parser GitObject
parseTree = parseHeader "tree" >> Tree . fromList <$> many' parseTreeEntry

parseTreeEntry :: Parser TreeEntry
parseTreeEntry = TreeEntry
    <$> (fst . head . readOct  <$> digit `manyTill` space)
    <*> (takeTill (==0)        <*  parseNull)
    <*> (encode . L.fromStrict <$> take 20)

parsePersonTime :: Parser PersonTime
parsePersonTime = PersonTime
    <$> (anyChar `manyTill` string " <")
    <*> (anyChar `manyTill` string "> ")
    <*> (digit   `manyTill` space)
    <*> parseRestOfLine

parseCommit :: Parser GitObject
parseCommit = parseHeader "commit" >> Commit
    <$>        ("tree"      *> space *> parseRef)
    <*>  many' ("parent"    *> space *> parseRef)
    <*>        ("author"    *> space *> parsePersonTime)
    <*>        ("committer" *> space *> parsePersonTime)
    <*>        parseMessage

parseTag :: Parser GitObject
parseTag = parseHeader "tag" >> Tag
    <$> ("object" *> space *> parseRef)
    <*> ("type"   *> space *> parseRestOfLine)
    <*> ("tag"    *> space *> parseRestOfLine)
    <*> ("tagger" *> space *> parsePersonTime)
    <*> parseMessage

parseObject :: Parser GitObject
parseObject = choice [parseBlob, parseTree, parseCommit, parseTag]
