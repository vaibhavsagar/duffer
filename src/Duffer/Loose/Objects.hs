{-# LANGUAGE RecordWildCards #-}

module Duffer.Loose.Objects where

import qualified Data.ByteString           as B
import qualified Data.ByteString.Builder   as BB
import qualified Data.ByteString.Lazy      as L
import qualified Data.ByteString.Lazy.UTF8 as UL (toString)

import Crypto.Hash            (hashWith)
import Crypto.Hash.Algorithms (SHA1)
import Data.Byteable
import Data.ByteString.UTF8   (fromString, toString)
import Data.ByteString.Base16 (decode)
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

-- A tree entry has permissions, a file/directory name, and a ref.
data TreeEntry = TreeEntry
    { entryPerms :: Int
    , entryName  :: B.ByteString
    , entryRef   :: Ref
    }
    deriving (Eq)

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
    show (Tree entries) = unlines $ map show $ toAscList entries
    show other          = UL.toString $ BB.toLazyByteString $ showContent other

instance Show PersonTime where
    show (PersonTime nm ml ti tz) = concat [nm, " <", ml, "> ", ti, " ", tz]

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
                if mode == 0o040000 || mode == 0o160000 then "/" else ""

instance Byteable GitObject where
    toBytes = L.toStrict . showObject

instance Byteable TreeEntry where
    toBytes (TreeEntry mode name sha1) = let
        mode' = fromString $ printf "%o" mode
        sha1' = fst $ decode sha1
        in B.concat [mode', " ", name, "\NUL", sha1']

sha1Path :: Ref -> Repo -> FilePath
sha1Path ref = let (sa:sb:suffix) = toString ref in
    flip (foldl (</>)) ["objects", [sa, sb], suffix]

-- Generate a stored representation of a git object.
showObject :: GitObject -> L.ByteString
showObject object = header `L.append` content
    where content    = BB.toLazyByteString $ showContent object
          header     = L.concat [objectType, " ", len, "\NUL"]
          objectType = case object of
            Blob{}   -> "blob"
            Tree{}   -> "tree"
            Commit{} -> "commit"
            Tag{}    -> "tag"
          len        = L.fromStrict $ fromString . show $ L.length content

showContent :: GitObject -> BB.Builder
showContent object = case object of
    Blob content -> BB.byteString content
    Tree entries -> mconcat $ map (BB.byteString . toBytes) $ toAscList entries
    Commit {..}  -> mconcat
        [                 "tree"      ?  treeRef
        , mconcat $ map ("parent"    ?) parentRefs
        ,                 "author"    ?  fromString (show authorTime)
        ,                 "committer" ?  fromString (show committerTime)
        ,                 "\n"        ,  BB.byteString message, "\n"
        ]
    Tag {..} -> mconcat
        [ "object" ?            objectRef
        , "type"   ? fromString objectType
        , "tag"    ? fromString tagName
        , "tagger" ? fromString (show tagger)
        , "\n"     , BB.byteString annotation, "\n"
        ]
    where (?) prefix value =
            mconcat $ map BB.byteString [prefix, " ", value, "\n"]

hash :: GitObject -> Ref
hash = fromString . show . hashWith (undefined :: SHA1) . toBytes
