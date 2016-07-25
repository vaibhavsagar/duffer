{-# LANGUAGE RecordWildCards #-}

module Duffer.Types where

import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as L

import Data.ByteString.UTF8        (fromString, toString)
import Data.ByteString.Base16.Lazy (encode, decode)
import Data.Digest.Pure.SHA        (sha1, bytestringDigest)
import Data.List                   (intercalate)
import Data.Set                    (Set, toAscList)
import System.FilePath             ((</>))
import Text.Printf                 (printf)

data GitObject
    = Blob {content :: !B.ByteString}
    | Tree {entries :: !(Set TreeEntry)}
    | Commit { treeRef       :: !Ref
             , parentRefs    :: ![Ref]
             , authorTime    :: !PersonTime
             , committerTime :: !PersonTime
             , message       :: !B.ByteString
             }
    | Tag { objectRef  :: !Ref
          , objectType :: !String
          , tagName    :: !String
          , tagger     :: !PersonTime
          , annotation :: !B.ByteString
          }

data TreeEntry = TreeEntry !Int !B.ByteString !Ref deriving (Eq)
data PersonTime = PersonTime { personName :: !String
                             , personMail :: !String
                             , personTime :: !String
                             , personTZ   :: !String
                             }

type Ref = L.ByteString
type Repo = String

instance Show GitObject where
    show object = case object of
        Blob {..} -> show content
        Tree {..} -> unlines $ map show $ toAscList entries
        Commit {..} -> concat
            [             "tree"      ?    showRef  treeRef
            , concatMap (("parent"    ?) . showRef) parentRefs
            ,             "author"    ?    show     authorTime
            ,             "committer" ?    show     committerTime
            ,             '\n'        :    toString message, "\n"
            ]
        Tag {..} -> concat
            [ "object" ? showRef objectRef
            , "type"   ? objectType
            , "tag"    ? tagName
            , "tagger" ? show tagger
            , '\n'     : toString annotation, "\n"
            ]
        where (?) prefix value = concat [prefix, ' ':value, "\n"] :: String

instance Show PersonTime where
    show (PersonTime nm ml ti tz) = concat [nm, " <", ml, "> ", ti, " ", tz]

instance Show TreeEntry where
    show (TreeEntry mode name sha1) = intercalate "\t" components
        where components = [octMode, entryType, showRef sha1, toString name]
              octMode = printf "%06o" mode :: String
              entryType = case mode of
                0o040000 -> "tree"
                0o160000 -> "commit"
                _        -> "blob"

instance Ord TreeEntry where
    compare t1 t2 = compare (sortableName t1) (sortableName t2)
        where sortableName (TreeEntry mode name _) = name `B.append`
                if mode == 0o040000 || mode == 0o160000 then "/" else ""

showRef :: Ref -> String
showRef = toString . L.toStrict

sha1Path :: Ref -> Repo -> FilePath
sha1Path ref = let (sa:sb:suffix) = showRef ref in
    flip (foldl (</>)) ["objects", [sa, sb], suffix]

-- Generate a stored representation of a git object.
showObject :: GitObject -> L.ByteString
showObject object = L.fromStrict $ uncurry makeStored $ case object of
    Blob content    -> ("blob",   content)
    Tree entries    -> ("tree",   B.concat $ map showEntry $ toAscList entries)
    commit@Commit{} -> ("commit", fromString $ show commit)
    tag@Tag{}       -> ("tag",    fromString $ show tag)
    where showEntry (TreeEntry mode name sha1) =
            let mode' = fromString $ printf "%o" mode
                sha1' = L.toStrict . fst $ decode sha1
            in B.concat [mode', " ", name, "\NUL", sha1']

makeStored :: B.ByteString -> B.ByteString -> B.ByteString
makeStored objectType content = header `B.append` content
    where header = B.concat [objectType, " ", len, "\NUL"]
          len    = fromString . show $ B.length content

hash :: GitObject -> Ref
hash = encode . bytestringDigest . sha1 . showObject

