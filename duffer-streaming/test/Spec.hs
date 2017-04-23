{-# LANGUAGE OverloadedStrings #-}

import Data.Foldable         (traverse_)
import Test.Hspec            (hspec, describe, it, runIO, shouldBe, SpecWith)
import Duffer.Pack           (getPackIndices, indexedEntryMap, packFile)
import Duffer.Pack.Streaming (separatePackFile)

main :: IO ()
main = hspec $
    testUnpackingAndWriting =<< runIO (getPackIndices "../.git/objects")

testUnpackingAndWriting :: [FilePath] -> SpecWith ()
testUnpackingAndWriting = describe "streaming packfiles" .
    traverse_ testAndWriteUnpacked

testAndWriteUnpacked :: FilePath -> SpecWith ()
testAndWriteUnpacked indexPath = describe (show (packFile indexPath)) $ do
    entryMap <- runIO $ indexedEntryMap indexPath
    it "can separate a streamed packfile" $ do
        separatedPackFile <- separatePackFile $ packFile indexPath
        separatedPackFile `shouldBe` entryMap
