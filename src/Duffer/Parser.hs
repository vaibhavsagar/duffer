module Duffer.Parser where

import qualified Data.ByteString      as B

import Data.Attoparsec.ByteString.Char8 hiding (takeTill)
import Prelude                          hiding (take)

import Data.Attoparsec.ByteString
import Data.ByteString.Base16     (encode)
import Data.ByteString.UTF8       (toString)
import Data.Set                   (fromList)
import Numeric                    (readOct)

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
parseRef = take 40 <* endOfLine

parseBlob :: Parser GitObject
parseBlob = Blob <$> takeByteString

parseTree :: Parser GitObject
parseTree = Tree . fromList <$> many' parseTreeEntry

parseTreeEntry :: Parser TreeEntry
parseTreeEntry = TreeEntry
    <$> (fst . head . readOct  <$> digit `manyTill` space)
    <*> (takeTill (==0)        <*  parseNull)
    <*> (encode                <$> take 20)

parsePersonTime :: Parser PersonTime
parsePersonTime = PersonTime
    <$> (anyChar `manyTill` string " <")
    <*> (anyChar `manyTill` string "> ")
    <*> (digit   `manyTill` space)
    <*> parseRestOfLine

parseCommit :: Parser GitObject
parseCommit = Commit
    <$>        ("tree"      *> space *> parseRef)
    <*>  many' ("parent"    *> space *> parseRef)
    <*>        ("author"    *> space *> parsePersonTime)
    <*>        ("committer" *> space *> parsePersonTime)
    <*>        parseMessage

parseTag :: Parser GitObject
parseTag = Tag
    <$> ("object" *> space *> parseRef)
    <*> ("type"   *> space *> parseRestOfLine)
    <*> ("tag"    *> space *> parseRestOfLine)
    <*> ("tagger" *> space *> parsePersonTime)
    <*> parseMessage

parseObject :: Parser GitObject
parseObject = choice
    [ "blob"   ? parseBlob
    , "tree"   ? parseTree
    , "commit" ? parseCommit
    , "tag"    ? parseTag
    ] where (?) oType parser = parseHeader oType >> parser
