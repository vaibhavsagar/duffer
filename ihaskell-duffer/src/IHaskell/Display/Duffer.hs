{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module IHaskell.Display.Duffer where

import qualified Data.ByteString.UTF8   as UB (toString)
import qualified ByteString.TreeBuilder as TB (toByteString)
import Data.ByteString.UTF8 (toString)
import Data.Byteable        (Byteable(..))
import Data.List            (intercalate)
import Data.Set             (toAscList)
import IHaskell.Display     (IHaskellDisplay(..), Display(..), plain)
import Text.Printf          (printf)

import Duffer.Loose.Objects (GitObjectGeneric(..), GitObject, TreeEntry(..)
                            ,PersonTime(..), EntryPermission(..), showContent)

wrap :: String -> IO Display
wrap = return . Display . return . plain

instance IHaskellDisplay GitObject where
    display (Tree entries) =
        wrap . unlines . map toDisplayString $ toAscList entries
    display other          =
        wrap . UB.toString . TB.toByteString $ showContent other

instance IHaskellDisplay PersonTime where display = wrap . toString . toBytes

instance IHaskellDisplay TreeEntry where
    display = wrap . toDisplayString

toDisplayString :: TreeEntry -> String
toDisplayString (TreeEntry mode name sha1) = intercalate "\t" components
    where components = [octMode, entryType, toString sha1, toString name]
          octMode    = printf "%06o" (fromEnum mode) :: String
          entryType  = case mode of
            Directory -> "tree"
            SubModule -> "commit"
            _         -> "blob"
