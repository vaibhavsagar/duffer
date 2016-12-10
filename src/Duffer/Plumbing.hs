module Duffer.Plumbing where

import qualified Data.ByteString      as B
import qualified Data.ByteString.UTF8 as BU
import qualified Data.Set             as S

import Control.Applicative        ((<|>))
import Control.Monad              (unless)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import System.Directory           (createDirectoryIfMissing, doesDirectoryExist
                                  ,doesFileExist, getDirectoryContents)
import System.FilePath            ((</>))
import Data.Attoparsec.ByteString (parseOnly)
import Data.Bool                  (bool)
import Data.ByteString.UTF8       (fromString)
import Data.List                  (isPrefixOf, foldl')

import Duffer.Loose
import Duffer.Loose.Objects
import Duffer.Loose.Parser
import Duffer.Unified
import Duffer.WithRepo

fuzzyReadObject :: String -> WithRepo (Maybe GitObject)
fuzzyReadObject search =
    let maybeRef = runMaybeT
            $   MaybeT (readRef search)
            <|> MaybeT (readRef $ "refs/heads"   </> search)
            <|> MaybeT (readRef $ "refs/remotes" </> search)
            <|> MaybeT (readRef $ "refs/tags"    </> search)
            <|> MaybeT (resolvePartialRef search)
    in maybe (return Nothing) readObject =<< maybeRef

resolveSymRef :: FilePath -> WithRepo (Maybe Ref)
resolveSymRef path = do
    symRefPath <- asks (</> path)
    liftIO (doesFileExist symRefPath) >>= bool
        (return Nothing)
        (do
            symRefContents <- liftIO $ readFile symRefPath
            let refPath = init $ drop 5 symRefContents
            resolveGitRef refPath)

resolveGitRef :: FilePath -> WithRepo (Maybe Ref)
resolveGitRef path = do
    refPath <- asks (</> path)
    liftIO (doesFileExist refPath) >>= bool
        (return Nothing)
        (do
            gitRefContents <- liftIO $ B.readFile refPath
            return $ either (const Nothing) Just $
                    parseOnly parseHexRef gitRefContents)

resolvePartialRef :: String -> WithRepo (Maybe Ref)
resolvePartialRef search = do
    let dir = take 2 search
    objectDir <- asks (</> "objects" </> dir)
    liftIO (doesDirectoryExist objectDir) >>= bool
        (return Nothing)
        (do
            let rest = drop 2 search
            possible <- liftIO (listDirectory objectDir)
            let filtered = filter (isPrefixOf rest) possible
            return $ bool
                Nothing
                (Just $ fromString $ dir ++ head filtered)
                (length filtered == 1))

initRepo :: WithRepo ()
initRepo = do
    path <- ask
    objectsPresent <- liftIO $ doesDirectoryExist $ path </> "objects"
    liftIO $ unless objectsPresent $ do
        mapM_ (createDirectoryIfMissing True)
            [ path </> "branches"
            , path </> "hooks"
            , path </> "info"
            , path </> "objects" </> "info"
            , path </> "objects" </> "pack"
            , path </> "refs"    </> "heads"
            , path </> "refs"    </> "tags"
            ]
        mapM_ (uncurry writeFile)
            [ (path </> "HEAD",   "ref: refs/heads/master\n")
            , (path </> "config", "")
            , (path </> "description", "")
            ]

writeTree :: FilePath -> WithRepo Ref
writeTree path = writeObject . Tree . S.fromList =<< mapM (\entry -> do
    fileExists <- liftIO $ doesFileExist      $ path </> entry
    dirExists  <- liftIO $ doesDirectoryExist $ path </> entry
    case (dirExists, fileExists) of
        (True, _) -> do
            tRef <- writeTree $ path </> entry
            return $ TreeEntry Directory (BU.fromString entry) tRef
        (_, True) -> do
            bContent <- liftIO $ B.readFile $ path </> entry
            bRef     <- writeObject $ Blob bContent
            return $ TreeEntry Regular (BU.fromString entry) bRef
        (False, False) -> error "what even"
    ) =<< liftIO (listDirectory path)
