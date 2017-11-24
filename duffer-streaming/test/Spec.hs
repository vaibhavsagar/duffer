import Control.Monad   (join)
import Data.Foldable   (traverse_)
import Data.IntMap     (toList)
import Pipes.Prelude   (toListM)
import System.FilePath ((</>))
import Test.Hspec      (hspec, describe, it, runIO, shouldBe, SpecWith)

import Duffer.Pack           (getPackIndices, indexedEntryMap, packFile)
import Duffer.Pack.Streaming (separatePackFile)

repo :: String
repo = "../.git"

main :: IO ()
main = hspec . describe "streaming packfiles" $
    traverse_ testAndWriteUnpacked =<<
        runIO (getPackIndices $ repo </> "objects")

testAndWriteUnpacked :: FilePath -> SpecWith ()
testAndWriteUnpacked indexPath = describe (show (packFile indexPath)) .
    it "can separate a streamed packfile" . join $ shouldBe
        <$> toListM (separatePackFile (packFile indexPath))
        <*> fmap toList (indexedEntryMap indexPath)
