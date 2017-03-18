module Duffer.Plumbing where

import qualified Data.ByteString      as B
import qualified Data.ByteString.UTF8 as BU
import qualified Data.Set             as S

import Control.Applicative       ((<|>))
import Control.Monad             (unless)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Data.Foldable             (traverse_)
import System.Directory          (createDirectoryIfMissing, doesDirectoryExist
                                 ,doesFileExist)
import System.FilePath           ((</>))
import Data.Bool                 (bool)
import Data.ByteString.UTF8      (fromString)
import Data.List                 (isPrefixOf)

import Duffer.Loose         (listDirectory)
import Duffer.Loose.Objects (GitObjectGeneric(..), GitObject, TreeEntry(..), EntryPermission(..)
                            ,Ref)
import Duffer.Pack          (getPackObjectRefs)
import Duffer.Unified       (readRef, readObject, writeObject)
import Duffer.WithRepo      (WithRepo, liftIO, ask, asks, localObjects)

fuzzyReadObject :: String -> WithRepo (Maybe GitObject)
fuzzyReadObject search = maybe (return Nothing) readObject =<< runMaybeT (
    MaybeT (readRef                      search) <|>
    MaybeT (readRef $ "refs/heads"   </> search) <|>
    MaybeT (readRef $ "refs/remotes" </> search) <|>
    MaybeT (readRef $ "refs/tags"    </> search) <|>
    MaybeT (resolvePartialRef            search))

resolvePartialRef :: String -> WithRepo (Maybe Ref)
resolvePartialRef search = runMaybeT
    $   MaybeT (localObjects $ resolvePartialLooseRef' search)
    <|> MaybeT (localObjects $ resolvePartialPackRef'  search)

resolvePartialLooseRef' :: String -> WithRepo (Maybe Ref)
resolvePartialLooseRef' search = do
    let dir = take 2 search
    objectDir <- asks (</> dir)
    liftIO (doesDirectoryExist objectDir) >>= bool
        (return Nothing)
        (do filtered <- filter (isPrefixOf (drop 2 search)) <$>
                liftIO (listDirectory objectDir)
            return $ bool
                Nothing
                (Just . fromString $ dir ++ head filtered)
                (length filtered == 1))

resolvePartialPackRef' :: String -> WithRepo (Maybe Ref)
resolvePartialPackRef' search = do
    matching <- S.filter (B.isPrefixOf (fromString search)) <$>
        getPackObjectRefs
    return $ bool Nothing (Just $ S.elemAt 0 matching) (S.size matching == 1)

initRepo :: WithRepo ()
initRepo = ask >>= \path -> do
    objectsPresent <- liftIO . doesDirectoryExist $ path </> "objects"
    liftIO . unless objectsPresent $ do
        traverse_ (createDirectoryIfMissing True)
            [ path </> "branches"
            , path </> "hooks"
            , path </> "info"
            , path </> "objects" </> "info"
            , path </> "objects" </> "pack"
            , path </> "refs"    </> "heads"
            , path </> "refs"    </> "tags"
            ]
        traverse_ (uncurry writeFile)
            [ (path </> "HEAD",        "ref: refs/heads/master\n")
            , (path </> "config",      "")
            , (path </> "description", "")
            ]

writeTree :: FilePath -> WithRepo Ref
writeTree path = writeObject . Tree . S.fromList =<< traverse (\entry -> do
    fileExists <- liftIO . doesFileExist      $ path </> entry
    dirExists  <- liftIO . doesDirectoryExist $ path </> entry
    case (dirExists, fileExists) of
        (True, _) -> do
            tRef <- writeTree $ path </> entry
            return $ TreeEntry Directory (BU.fromString entry) tRef
        (_, True) -> do
            bContent <- liftIO . B.readFile $ path </> entry
            bRef     <- writeObject $ Blob bContent
            return $ TreeEntry Regular (BU.fromString entry) bRef
        (False, False) -> error "what even"
    ) =<< liftIO (listDirectory path)
