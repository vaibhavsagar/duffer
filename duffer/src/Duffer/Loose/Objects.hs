{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StrictData #-}

module Duffer.Loose.Objects (module Duffer.Loose.Objects) where

import qualified Data.ByteString        as B
import qualified Data.ByteString.Base16 as B16 (decode)
import qualified ByteString.TreeBuilder as TB
import qualified Data.ByteString.UTF8   as UB (toString)

import Crypto.Hash             (hashWith)
import Crypto.Hash.Algorithms  (SHA1)
import Data.Bool               (bool)
import Data.ByteArray.Encoding (Base(Base16), convertToBase)
import Data.Byteable           (Byteable(toBytes))
import Data.ByteString.UTF8    (fromString, toString)
import Data.Function           (on)
import Data.List               (intercalate)
import Data.Monoid             (mconcat)
import Data.Set                (Set, toAscList)
import Data.String             (IsString)
import System.FilePath         ((</>))
import Text.Printf             (printf)

data GitObjectGeneric ref container entries
    = Blob {blobContent :: B.ByteString}
    | Tree {treeEntries :: container entries}
    | Commit
        { commitTreeRef    :: ref
        , commitParentRefs :: [ref]
        , commitAuthor     :: PersonTime
        , commitCommitter  :: PersonTime
        , commitSignature  :: Maybe B.ByteString
        , commitMessage    :: B.ByteString
        }
    | Tag
        { tagObjectRef  :: ref
        , tagObjectType :: B.ByteString
        , tagName       :: B.ByteString
        , tagTagger     :: PersonTime
        , tagAnnotation :: B.ByteString
        }
    deriving (Eq)

type GitObject = GitObjectGeneric Ref Set TreeEntry

-- A tree entry has permissions, a file/directory name, and a ref.
data TreeEntry = TreeEntry
    { entryPerms :: EntryPermission
    , entryName  :: B.ByteString
    , entryRef   :: Ref
    }

data PersonTime = PersonTime
    { personName :: B.ByteString
    , personMail :: B.ByteString
    , personTime :: B.ByteString
    , personTZ   :: B.ByteString
    }
    deriving (Eq)

data EntryPermission
    = Directory
    | Regular
    | Executable
    | SymbolicLink
    | SubModule
    deriving (Show, Eq)

type Ref  = B.ByteString
type Repo = FilePath

instance Show GitObject where
    show (Tree entries) = unlines . map show $ toAscList entries
    show other          = UB.toString . TB.toByteString $ showContent other

instance Byteable PersonTime where
    toBytes (PersonTime name mail time zone) =
        B.concat [name, " <", mail, "> ", time, " ", zone]

instance Show PersonTime where show = toString . toBytes

instance Show TreeEntry where
    show (TreeEntry mode name sha1) = intercalate "\t" components
        where components = [octMode, entryType, toString sha1, toString name]
              octMode    = printf "%06o" (fromEnum mode) :: String
              entryType  = case mode of
                Directory -> "tree"
                SubModule -> "commit"
                _         -> "blob"

instance Eq  TreeEntry where (==)    = (==)    `on` sortableName
instance Ord TreeEntry where compare = compare `on` sortableName

sortableName :: TreeEntry -> B.ByteString
sortableName TreeEntry{..} = entryName `B.append`
    bool "" "/" (entryPerms == Directory || entryPerms == SubModule)

instance Byteable TreeEntry where
    toBytes (TreeEntry mode name sha1) = let
        mode' = fromString $ printf "%o" (fromEnum mode)
        sha1' = fst $ B16.decode sha1
        in B.concat [mode', " ", name, "\NUL", sha1']

instance Byteable GitObject where toBytes = showObject

instance Enum EntryPermission where
    fromEnum p = case p of
        Directory    -> 0o040000
        Regular      -> 0o100644
        Executable   -> 0o100755
        SymbolicLink -> 0o120000
        SubModule    -> 0o160000
    toEnum i = case i of
        0o040000 -> Directory
        0o100644 -> Regular
        0o100755 -> Executable
        0o120000 -> SymbolicLink
        0o160000 -> SubModule
        _        -> error "invalid permission"

sha1Path :: Ref -> Repo -> FilePath
sha1Path ref = let (sa:sb:suffix) = toString ref in
    flip (foldl (</>)) [[sa, sb], suffix]

-- Generate a stored representation of a git object.
showObject :: GitObject -> B.ByteString
showObject gitObject = header `B.append` content
    where content    = TB.toByteString $ showContent gitObject
          header     = B.concat [objectType gitObject, " ", len, "\NUL"]
          len        = fromString . show $ B.length content

objectType :: IsString a => GitObjectGeneric r c e -> a
objectType someObject = case someObject of
    Blob{}   -> "blob"
    Tree{}   -> "tree"
    Commit{} -> "commit"
    Tag{}    -> "tag"

showContent :: GitObject -> TB.Builder
showContent gitObject = case gitObject of
    Blob content -> TB.byteString content
    Tree entries -> mconcat . map (TB.byteString . toBytes) $ toAscList entries
    Commit {..}  -> mconcat
        [                "tree"      ?                commitTreeRef
        , mconcat $ map ("parent"    ?)               commitParentRefs
        ,                "author"    ?  toBytes       commitAuthor
        ,                "committer" ?  toBytes       commitCommitter
        , maybe mempty  ("gpgsig"    ?)               commitSignature
        ,                "\n"        ,  TB.byteString commitMessage
        ]
    Tag {..} -> mconcat
        [ "object" ?               tagObjectRef
        , "type"   ?               tagObjectType
        , "tag"    ?               tagName
        , "tagger" ?       toBytes tagTagger
        , "\n"     , TB.byteString tagAnnotation
        ]
    where (?) key value = mconcat $ map TB.byteString [key, " ", value, "\n"]

hashSHA1 :: B.ByteString -> Ref
hashSHA1 = convertToBase Base16 . hashWith (undefined :: SHA1)

hash :: GitObject -> Ref
hash = hashSHA1 . toBytes
