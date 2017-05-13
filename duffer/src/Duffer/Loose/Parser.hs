module Duffer.Loose.Parser (module Duffer.Loose.Parser) where

import Prelude                          hiding (take)

import Data.ByteString                  (ByteString, cons, pack, find
                                        ,intercalate)
import Control.Applicative              ((<|>))
import Data.Attoparsec.ByteString       (Parser, anyWord8, notInClass)
import Data.Attoparsec.ByteString.Char8 (anyChar, char, char8, choice, digit
                                        ,isDigit, space, string, manyTill'
                                        ,endOfLine, takeByteString, takeTill
                                        ,takeWhile1, many', take, option)
import Data.ByteString.Base16           (encode)
import Data.ByteString.UTF8             (fromString)
import Data.Set                         (fromList)
import Numeric                          (readOct)

import Duffer.Loose.Objects (GitObjectGeneric(..), GitObject, TreeEntry(..)
                            ,PersonTime(..), Ref)

parseNull :: Parser Char
parseNull = char '\NUL'

parseHeader :: ByteString -> Parser String
parseHeader = (*> digit `manyTill'` parseNull) . (*> space) . string

parseRestOfLine :: Parser ByteString
parseRestOfLine = takeTill (=='\n') <* endOfLine

parseMessage :: Parser ByteString
parseMessage = endOfLine *> takeByteString

parseSignature :: Parser ByteString
parseSignature = do
    begin <- string "-----BEGIN PGP SIGNATURE-----" <* endOfLine
    sig <- parseRestOfLine `manyTill'` string end
    return . intercalate "\n" $ begin:sig ++ [end]
    where end = " -----END PGP SIGNATURE-----"

parseTimeZone :: Parser ByteString
parseTimeZone = cons <$> (char8 '+' <|> char8 '-') <*> takeWhile1 isDigit

validateRef :: Monad m => ByteString -> m Ref
validateRef possibleRef = maybe
    (return possibleRef)
    (const $ fail "invalid ref")
    $ find (notInClass "0-9a-f") possibleRef

parseHexRef, parseBinRef :: Parser Ref
parseHexRef = validateRef =<< take 40
parseBinRef = validateRef =<< encode <$> take 20

parseBlob :: Parser GitObject
parseBlob = Blob <$> takeByteString

parseTree :: Parser GitObject
parseTree = Tree . fromList <$> many' parseTreeEntry

parseTreeEntry :: Parser TreeEntry
parseTreeEntry = TreeEntry <$> parsePerms <*> parseName <*> parseBinRef
    where
        parsePerms = toEnum . fst . head . readOct <$> digit `manyTill'` space
        parseName  = takeWhile1 (/='\NUL')         <*  parseNull

parsePersonTime :: Parser PersonTime
parsePersonTime = PersonTime
    <$> (pack       <$> anyWord8 `manyTill'` string " <")
    <*> (pack       <$> anyWord8 `manyTill'` string "> ")
    <*> (fromString <$> digit    `manyTill'` space)
    <*> parseTimeZone

parseCommit :: Parser GitObject
parseCommit = Commit
    <$>          "tree"      .= parseHexRef
    <*> many'   ("parent"    .= parseHexRef)
    <*>          "author"    .= parsePersonTime
    <*>          "committer" .= parsePersonTime
    <*> perhaps ("gpgsig"    .= parseSignature)
    <*>                         parseMessage
    where
        perhaps         = option Nothing . fmap Just
        field .= parser = string field *> space *> parser <* endOfLine

parseTag :: Parser GitObject
parseTag = Tag
    <$>  "object" =: parseHexRef
    <*>  "type"   =: parseType
    <*> ("tag"    *> space *> parseRestOfLine)
    <*>  "tagger" =: parsePersonTime
    <*>  parseMessage
    where
        parseType = choice $ map string ["blob", "tree", "commit", "tag"]
        field =: parser = string field *> space *> parser <* endOfLine

parseObject :: Parser GitObject
parseObject = choice
    [ "blob"   .*> parseBlob
    , "tree"   .*> parseTree
    , "commit" .*> parseCommit
    , "tag"    .*> parseTag
    ] where (.*>) oType = (parseHeader oType *>)

parseSymRef :: Parser String
parseSymRef = string "ref:" *> space *> anyChar `manyTill'` endOfLine
