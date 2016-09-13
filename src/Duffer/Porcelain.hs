module Duffer.Porcelain where

import qualified Data.ByteString as B

import Control.Applicative        ((<|>))
import Control.Monad.IO.Class     (liftIO)
import Control.Monad.Trans.Reader (asks)
import System.Directory           (doesDirectoryExist, doesFileExist
                                  ,getDirectoryContents)
import System.FilePath            ((</>))
import Data.Attoparsec.ByteString (parseOnly)
import Data.ByteString.UTF8       (fromString)
import Data.List                  (isPrefixOf)

import Duffer.Loose
import Duffer.Loose.Objects
import Duffer.Loose.Parser

fuzzyReadObject :: String -> WithRepo (Maybe GitObject)
fuzzyReadObject search = do
    symRef     <- resolveSymRef search
    branchRef  <- resolveGitRef $ "refs/heads"   </> search
    remoteRef  <- resolveGitRef $ "refs/remotes" </> search
    tagRef     <- resolveGitRef $ "refs/tags"    </> search
    partialRef <- resolvePartialRef search
    let result = foldl (<|>) Nothing
            [symRef, branchRef, remoteRef, tagRef, partialRef]
    maybe (return Nothing) readObject result

resolveSymRef :: FilePath -> WithRepo (Maybe Ref)
resolveSymRef path = do
    symRefPath <- asks (</> path)
    exists  <- liftIO $ doesFileExist symRefPath
    if exists
        then do
            symRefContents <- liftIO $ readFile symRefPath
            let refPath = init $ drop 5 symRefContents
            resolveGitRef refPath
        else return Nothing

resolveGitRef :: FilePath -> WithRepo (Maybe Ref)
resolveGitRef path = do
    refPath <- asks (</> path)
    exists  <- liftIO $ doesFileExist refPath
    if exists
        then do
            gitRefContents <- liftIO $ B.readFile refPath
            return $ either (const Nothing) Just $
                    parseOnly parseHexRef gitRefContents
        else return Nothing

resolvePartialRef :: String -> WithRepo (Maybe Ref)
resolvePartialRef search = do
    let dir = take 2 search
    objectDir <- asks (</> "objects" </> dir)
    exists  <- liftIO $ doesDirectoryExist objectDir
    if exists
        then do
            let rest = drop 2 search
            possible <- liftIO $ getDirectoryContents objectDir
            let filtered = filter (isPrefixOf rest) possible
            if length filtered == 1
                then return $ Just $ fromString $ dir ++ head filtered
                else return Nothing
        else return Nothing
