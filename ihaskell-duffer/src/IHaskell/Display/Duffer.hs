{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module IHaskell.Display.Duffer where

import Data.ByteString.UTF8   (toString)
import ByteString.TreeBuilder (toByteString)
import Data.Byteable          (Byteable(..))
import Data.List              (intercalate)
import Data.Set               (toAscList)
import IHaskell.Display       (IHaskellDisplay(..), Display(..), plain)
import Text.Printf            (printf)

import Duffer.Loose.Objects   (GitObjectGeneric(..), GitObject, TreeEntry(..)
                              ,PersonTime(..), EntryPermission(..), Tree(..)
                              ,showContent)

wrap :: String -> IO Display
wrap = return . Display . return . plain

instance IHaskellDisplay GitObject where
    display (GitTree (Tree entries)) =
        wrap . unlines . map toDisplayString $ toAscList entries
    display other          =
        wrap . toString . toByteString $ showContent other

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
