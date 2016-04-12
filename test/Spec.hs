{-# LANGUAGE OverloadedStrings #-}

import Duffer
import Test.Hspec
import System.Process
import Data.ByteString (hGetContents)
import Data.ByteString.UTF8 (fromString, toString)

main :: IO ()
main = do

    blobs   <- objectsOfType "blob"
    trees   <- objectsOfType "tree"

    hspec $ do
        describe "blob" $
            it "correctly parses and hashes all blobs" $
                mapM_ (readHashObject ".git") blobs
        describe "tree" $
            it "correctly parses and hashes all trees" $
                mapM_ (readHashObject ".git") trees

objectsOfType :: String -> IO [String]
objectsOfType objectType = do
    (_, Just h1, _, _) <- createProcess (shell "git rev-list --objects --all") {std_out = CreatePipe}
    (_, Just h2, _, _) <- createProcess (shell "git cat-file --batch-check='%(objectname) %(objecttype) %(rest)'") {std_out = CreatePipe, std_in = UseHandle h1}
    (_, Just h3, _, _) <- createProcess (shell $ "grep '^[^ ]* " ++ objectType ++ "'") {std_out = CreatePipe, std_in = UseHandle h2}
    (_, Just h4, _, _) <- createProcess (shell "cut -d' ' -f1") {std_out = CreatePipe, std_in = UseHandle h3}
    objectLines <- hGetContents h4
    return $ lines (toString objectLines)

readHashObject :: String -> String -> Expectation
readHashObject path sha1 =
    readObject path sha1 >>= \object -> hash (storedObject object) `shouldBe` sha1
