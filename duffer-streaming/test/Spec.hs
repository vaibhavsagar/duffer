import Control.Monad         (join)
import Data.Foldable         (traverse_)
import Data.IntMap           (toList)
import Test.Hspec            (hspec, describe, it, runIO, shouldBe, SpecWith)
import Pipes.Prelude         (toListM)
import Duffer.Pack           (getPackIndices, indexedEntryMap, packFile)
import Duffer.Pack.Streaming (separatePackFile)

main :: IO ()
main = hspec . describe "streaming packfiles" $
    traverse_ testAndWriteUnpacked =<< runIO (getPackIndices "../.git/objects")

testAndWriteUnpacked :: FilePath -> SpecWith ()
testAndWriteUnpacked indexPath = describe (show (packFile indexPath)) $
    it "can separate a streamed packfile" . join $ shouldBe
        <$> toListM (separatePackFile (packFile indexPath))
        <*> fmap toList (indexedEntryMap indexPath)
