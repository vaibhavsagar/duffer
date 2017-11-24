{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad        (zipWithM_)
import Data.Aeson           (encode, decode, ToJSON, FromJSON)
import Data.ByteString      (hGetContents)
import Data.ByteString.UTF8 (lines, toString)
import Data.Coerce          (Coercible, coerce)
import Data.Foldable        (traverse_)
import Data.Maybe           (fromJust)
import System.Process       (CreateProcess(..), StdStream(..), createProcess
                            ,shell)
import Test.Hspec           (hspec, expectationFailure, parallel, describe, it
                            ,shouldBe, Expectation, SpecWith)

import Prelude hiding (lines)

import Duffer.Unified       (readObject)
import Duffer.Loose.Objects (Ref)
import Duffer.WithRepo      (withRepo)
import Duffer.JSON          (GitObjectJSON(..), RefJSON(..))

repo :: String
repo = "../.git"

gitDir :: String
gitDir = "GIT_DIR=" ++ repo ++ " "

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
        traverse_ (decodeEncodeObject repo)

decodeEncodeObject :: FilePath -> Ref -> Expectation
decodeEncodeObject path ref = withRepo path (readObject ref) >>= maybe
    (expectationFailure $ toString ref ++ "not read")
    ((roundTrip . GitObjectJSON) >>= shouldBe)

testRefs :: [Ref] -> SpecWith ()
testRefs = it "correctly decodes and encodes all refs" .
    traverse_ ((roundTrip . RefJSON) >>= shouldBe)

roundTrip :: forall a b. (Coercible a b, FromJSON a, ToJSON a) => a -> b
roundTrip = coerce @a . fromJust . decode . encode

objectsOfType :: String -> IO [Ref]
objectsOfType objectType = fmap lines $
    cmd (gitDir ++ "git rev-list --objects --all")
    >|> (gitDir ++
        "git cat-file --batch-check='%(objectname) %(objecttype) %(rest)'")
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
