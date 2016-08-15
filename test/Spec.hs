{-# LANGUAGE OverloadedStrings #-}

import Data.ByteString (readFile, hGetContents)
import Test.Hspec
import Control.Monad (zipWithM_)
import System.Process
import System.FilePath
import System.Directory
import Control.Monad.Trans.Reader (runReaderT)
import Data.ByteString.UTF8 (lines)
import GHC.IO.Handle (Handle)
import Prelude hiding (lines, readFile)

import Duffer
import Duffer.Pack
import Duffer.Pack.File (resolveAll')
import Duffer.Pack.Parser
import Duffer.Pack.Entries

main :: IO ()
main = do
    filenames <- getPackIndexes ".git"
    hspec . parallel $ describe "unpacking packfiles" $
        mapM_ testUnpacked filenames

    let objectTypes    =  ["blob", "tree", "commit", "tag"]
    partitionedObjects <- mapM objectsOfType objectTypes
    hspec . parallel $ describe "reading loose objects" $
        zipWithM_ describeReadingAll objectTypes partitionedObjects

describeReadingAll :: String -> [Ref] -> SpecWith ()
describeReadingAll oType objects = describe oType $
    readAll ("correctly parses and hashes all " ++ oType ++ "s") objects
    where readAll desc os = it desc (mapM_ (readHashObject ".git") os)

testUnpacked :: FilePath -> SpecWith ()
testUnpacked indexPath = it (show indexPath) $ do
    index     <- parsedIndex <$> readFile        indexPath
    let refs  =  map (snd . toAssoc)             index
    objects   <- resolveAll                      indexPath
    objects'  <- resolveAll' <$> indexedEntryMap indexPath
    let write =  flip runReaderT ".git" . writeObject
    shouldMatchList objects'            objects
    shouldMatchList refs (map hash      objects)
    shouldMatchList refs =<< mapM write objects

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
(>|>) handle cmd = withPipe cmd =<< handle
    where withPipe cmd pipe = createProcess (shell cmd)
            {std_out = CreatePipe, std_in = UseHandle pipe} >>=
            \(_, Just handle, _, _) -> return handle

getPackIndexes :: String -> IO [FilePath]
getPackIndexes path = let packFilePath = path ++ "/objects/pack" in
    map (combine packFilePath) .  filter (\f -> takeExtension f == ".idx") <$>
    getDirectoryContents packFilePath

readHashObject :: String -> Ref -> Expectation
readHashObject path sha1 =
    hash <$> runReaderT (readObject sha1) path `shouldReturn` sha1
