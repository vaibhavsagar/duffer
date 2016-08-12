{-# LANGUAGE RecordWildCards #-}

module Duffer.Loose.Objects where

import qualified Crypto.Hash.SHA1 as SHA1 (hash)
import qualified Data.ByteString  as B

import Data.ByteString.UTF8   (fromString, toString)
import Data.ByteString.Base16 (encode, decode)
import Data.List              (intercalate)
import Data.Set               (Set, toAscList)
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
        , objectType :: String
        , tagName    :: String
        , tagger     :: PersonTime
        , annotation :: B.ByteString
        }
    deriving (Eq)

data TreeEntry = TreeEntry Int B.ByteString Ref deriving (Eq)
data PersonTime = PersonTime
    { personName :: String
    , personMail :: String
    , personTime :: String
    , personTZ   :: String
    }
    deriving (Eq)

type Ref  = B.ByteString
type Repo = FilePath

instance Show GitObject where
    show = toString . showContent

instance Show PersonTime where
    show (PersonTime nm ml ti tz) = concat [nm, " <", ml, "> ", ti, " ", tz]

instance Show TreeEntry where
    show (TreeEntry mode name sha1) = intercalate "\t" components
        where components = [octMode, entryType, sha1', toString name]
              octMode    = printf "%06o" mode :: String
              sha1'      = toString $ encode sha1
              entryType  = case mode of
                0o040000 -> "tree"
                0o160000 -> "commit"
                _        -> "blob"

instance Ord TreeEntry where
    compare t1 t2 = compare (sortableName t1) (sortableName t2)
        where sortableName (TreeEntry mode name _) = name `B.append`
                if mode == 0o040000 || mode == 0o160000 then "/" else ""

sha1Path :: Ref -> Repo -> FilePath
sha1Path ref = let (sa:sb:suffix) = toString ref in
    flip (foldl (</>)) ["objects", [sa, sb], suffix]

-- Generate a stored representation of a git object.
showObject :: GitObject -> B.ByteString
showObject object = B.concat [header, content]
    where content    = showContent object
          header     = B.concat [objectType, " ", len, "\NUL"]
          objectType = case object of
            Blob{}   -> "blob"
            Tree{}   -> "tree"
            Commit{} -> "commit"
            Tag{}    -> "tag"
          len        = fromString . show $ B.length content

showContent :: GitObject -> B.ByteString
showContent object = case object of
    Blob content -> content
    Tree entries -> B.concat $ map showEntry $ toAscList entries
    Commit {..}  -> B.concat
        [                 "tree"      ?  treeRef
        , B.concat $ map ("parent"    ?) parentRefs
        ,                 "author"    ?  fromString (show authorTime)
        ,                 "committer" ?  fromString (show committerTime)
        ,                 "\n"        ,  message, "\n"
        ]
    Tag {..} -> B.concat
        [ "object" ?            objectRef
        , "type"   ? fromString objectType
        , "tag"    ? fromString tagName
        , "tagger" ? fromString (show tagger)
        , "\n"     , annotation, "\n"
        ]
    where (?) prefix value = B.concat [prefix, " ", value, "\n"]
          showEntry (TreeEntry mode name sha1) =
            let mode' = fromString $ printf "%o" mode
                sha1' = fst $ decode sha1
            in B.concat [mode', " ", name, "\NUL", sha1']

hash :: GitObject -> Ref
hash = encode . SHA1.hash . showObject
