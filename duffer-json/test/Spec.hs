{-# LANGUAGE TypeApplications #-}

import Control.Monad              (zipWithM_)
import Data.Aeson                 (encode, decode)
import Data.ByteString            (hGetContents)
import Data.ByteString.UTF8       (lines, toString)
import Data.Coerce                (coerce)
import Data.Foldable              (traverse_)
import Data.Maybe                 (fromJust)
import GHC.IO.Handle              (Handle)
import System.Process             (CreateProcess(..), StdStream(..)
                                  ,createProcess, shell)
import Test.Hspec                 (hspec, expectationFailure, parallel, describe
                                  ,it, shouldBe, Expectation, SpecWith)

import Prelude hiding (lines, readFile)

import Duffer.Unified        (readObject)
import Duffer.Loose.Objects  (Ref)
import Duffer.WithRepo       (withRepo)
import Duffer.JSON

main :: IO ()
main = do
    let objectTypes = ["blob", "tree", "commit", "tag"]
    allTypesObjects <- traverse objectsOfType objectTypes
    hspec . parallel . describe "JSON" $
        testJSON objectTypes allTypesObjects

testJSON :: [String] -> [[Ref]] -> SpecWith ()
testJSON types partitionedObjects = describe "decoding and encoding" $
    zipWithM_ describeDecodingEncodingAll types partitionedObjects

describeDecodingEncodingAll :: String -> [Ref] -> SpecWith ()
describeDecodingEncodingAll oType =
    readAll ("correctly decodes and encodes all " ++ oType ++ "s")
    where readAll desc os = it desc (traverse_ (decodeEncodeObject "../.git") os)

objectsOfType :: String -> IO [Ref]
objectsOfType objectType = fmap lines $
    cmd "git rev-list --objects --all"
    >|> "git cat-file --batch-check='%(objectname) %(objecttype) %(rest)'"
    >|> ("grep '^[^ ]* " ++ objectType ++ "'")
    >|> "cut -d' ' -f1"
    >>= hGetContents

cmd :: String -> IO Handle
cmd command = createProcess (shell command) {std_out = CreatePipe} >>=
    \(_, Just handle, _, _) -> return handle

(>|>) :: IO Handle -> String -> IO Handle
(>|>) handle command = withPipe =<< handle
    where withPipe pipe = createProcess (shell command)
            {std_out = CreatePipe, std_in = UseHandle pipe} >>=
            \(_, Just handle', _, _) -> return handle'

decodeEncodeObject :: FilePath -> Ref -> Expectation
decodeEncodeObject path ref = withRepo path (readObject ref) >>= maybe
    (failureNotFound $ toString ref)
    (\object -> roundtrip object `shouldBe` object)
    where roundtrip =
            coerce @GitObjectJSON . fromJust . decode . encode . GitObjectJSON

failureNotFound :: String -> Expectation
failureNotFound string = expectationFailure $ string ++ " not found"
