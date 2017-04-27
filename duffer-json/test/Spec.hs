{-# LANGUAGE TypeApplications #-}

import Control.Monad              (zipWithM_)
import Data.Aeson                 (encode, decode)
import Data.ByteString            (hGetContents)
import Data.ByteString.UTF8       (lines, toString)
import Data.Coerce                (coerce)
import Data.Foldable              (traverse_)
import Data.Maybe                 (fromJust)
import System.Process             (CreateProcess(..), StdStream(..)
                                  ,createProcess, shell)
import Test.Hspec                 (hspec, expectationFailure, parallel, describe
                                  ,it, shouldBe, Expectation, SpecWith)

import Prelude hiding (lines, readFile)

import Duffer.Unified        (readObject)
import Duffer.Loose.Objects  (Ref)
import Duffer.WithRepo       (withRepo)
import Duffer.JSON           (GitObjectJSON(..), RefJSON(..))

main :: IO ()
main = let objectTypes = ["blob", "tree", "commit", "tag"] in
    traverse objectsOfType objectTypes >>=
        hspec . parallel . describe "JSON" . testJSON objectTypes

testJSON :: [String] -> [[Ref]] -> SpecWith ()
testJSON types partitionedRefs = describe "decoding and encoding" $ do
    zipWithM_ describeDecodingEncodingAll types partitionedRefs
    testRefs $ concat partitionedRefs

describeDecodingEncodingAll :: String -> [Ref] -> SpecWith ()
describeDecodingEncodingAll oType =
    it ("correctly decodes and encodes all " ++ oType ++ "s") .
        traverse_ (decodeEncodeObject "../.git")

decodeEncodeObject :: FilePath -> Ref -> Expectation
decodeEncodeObject path ref = withRepo path (readObject ref) >>= maybe
    (expectationFailure $ toString ref ++ "not read")
    (flip shouldBe <*> roundtrip)
    where roundtrip =
            coerce @GitObjectJSON . fromJust . decode . encode . GitObjectJSON

testRefs :: [Ref] -> SpecWith ()
testRefs = it "correctly decodes and encodes all refs" .
    traverse_ decodeEncodeRef

decodeEncodeRef :: Ref -> Expectation
decodeEncodeRef = flip shouldBe <*> roundtrip
    where roundtrip = coerce @RefJSON . fromJust . decode . encode . RefJSON

objectsOfType :: String -> IO [Ref]
objectsOfType objectType = fmap lines $
    cmd "git rev-list --objects --all"
    >|> "git cat-file --batch-check='%(objectname) %(objecttype) %(rest)'"
    >|> ("grep '^[^ ]* " ++ objectType ++ "'")
    >|> "cut -d' ' -f1"
    >>= hGetContents
    where
        cmd command = createProcess (shell command) {std_out = CreatePipe} >>=
            \(_, Just handle, _, _) -> return handle
        (>|>) handle command = withPipe =<< handle
            where withPipe pipe = createProcess (shell command)
                    {std_out = CreatePipe, std_in = UseHandle pipe} >>=
                    \(_, Just handle', _, _) -> return handle'
