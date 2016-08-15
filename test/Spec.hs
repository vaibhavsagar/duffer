{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString as B

import Data.Attoparsec.ByteString
import Test.Hspec
import Control.Monad (forM_)
import System.Process
import System.FilePath
import System.Directory
import Control.Monad.Trans.Reader (runReaderT)
import Data.ByteString (hGetContents)
import Data.ByteString.UTF8 (lines)
import Prelude hiding (lines)

import Duffer
import Duffer.Pack
import Duffer.Pack.File (resolveAll')
import Duffer.Pack.Parser
import Duffer.Pack.Entries

main :: IO ()
main = do

    blobs   <- objectsOfType "blob"
    trees   <- objectsOfType "tree"
    commits <- objectsOfType "commit"
    tags    <- objectsOfType "tag"

    let readCurrent = readHashObject ".git"

    hspec . parallel $ describe "reading pack indexes" $ do
        filenames <- runIO $ getPackIndexes ".git"
        forM_ filenames $ \indexPath -> it (show indexPath) $ do
            content <- B.readFile indexPath
            let entries = either error id (parseOnly parsePackIndex content)
            let refs = map (snd . toAssoc) entries
            objects  <- resolveAll  indexPath
            objects' <- resolveAll' <$> indexedEntryMap indexPath
            objects `shouldMatchList` objects'
            let writeLoose = flip runReaderT ".git" . writeObject
            resolvedRefs <- mapM writeLoose objects
            resolvedRefs `shouldMatchList` refs

    hspec . parallel $
        describe "reading" $ do
            describe "blob" $
                it "correctly parses and hashes all blobs" $
                    mapM_ readCurrent blobs
            describe "tree" $
                it "correctly parses and hashes all trees" $
                    mapM_ readCurrent trees
            describe "commit" $
                it "correctly parses and hashes all commits" $
                    mapM_ readCurrent commits
            describe "tag" $
                it "correctly parses and hashes all tags" $
                    mapM_ readCurrent tags

objectsOfType :: String -> IO [Ref]
objectsOfType objectType = do
    (_, Just h1, _, _) <- createProcess (shell "git rev-list --objects --all") {std_out = CreatePipe}
    (_, Just h2, _, _) <- createProcess (shell "git cat-file --batch-check='%(objectname) %(objecttype) %(rest)'") {std_out = CreatePipe, std_in = UseHandle h1}
    (_, Just h3, _, _) <- createProcess (shell $ "grep '^[^ ]* " ++ objectType ++ "'") {std_out = CreatePipe, std_in = UseHandle h2}
    (_, Just h4, _, _) <- createProcess (shell "cut -d' ' -f1") {std_out = CreatePipe, std_in = UseHandle h3}
    Data.ByteString.UTF8.lines  <$> hGetContents h4

getPackIndexes :: String -> IO [FilePath]
getPackIndexes path = let packFilePath = path ++ "/objects/pack" in
    map (combine packFilePath) .  filter (\f -> takeExtension f == ".idx") <$>
    getDirectoryContents packFilePath

readHashObject :: String -> Ref -> Expectation
readHashObject path sha1 =
    hash <$> runReaderT (readObject sha1) path `shouldReturn` sha1
