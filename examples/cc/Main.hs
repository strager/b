{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent.MVar
import Control.Exception (throwIO)
import Control.Monad
import Control.Monad.IO.Class
import Data.Semigroup
import Data.Text (Text)
import Data.Typeable
import Data.Typeable.Internal (TypeRep(TypeRep))
import Filesystem (createTree)
import Filesystem.Path.CurrentOS (FilePath, (</>))
import Prelude hiding (FilePath, readFile)
import System.Exit
import System.Process (rawSystem)

import qualified Data.Binary.Get as Binary
import qualified Data.Binary.Put as Binary
import qualified Data.ByteString.Lazy as BSLazy
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Filesystem as FS
import qualified Filesystem.Path.CurrentOS as Path

import B.File
import B.Monad
import B.Oracle.Binary
import B.Question
import B.RuleDatabase (RuleDatabase)

import qualified B.Oracle as Oracle
import qualified B.Oracle.InMemory as InMemory
import qualified B.Oracle.InMemory.Pure as OraclePure
import qualified B.RuleDatabase as RuleDatabase

runSystem
  :: (MonadIO m, Typeable1 m)
  => String    -- ^ Program.
  -> [String]  -- ^ Arguments.
  -> BuildRule m ()
runSystem prog args = liftIO $ do
  liftIO . putStrLn $ prog ++ " " ++ unwords args
  exit <- rawSystem prog args
  case exit of
    ExitSuccess -> return ()
    _ -> throwIO exit

-- FIXME Does not handle:
--
-- > ("a.b.c" `hasExtenions" ["c"]) == True
hasExtensions :: FilePath -> [Text] -> Bool
hasExtensions path expectedExtensions
  = Path.extensions path == expectedExtensions

-- TODO Handle escapes (spaces!) and special characters.
parseDepFileDeps :: Text -> [FilePath]
parseDepFileDeps
  = map Path.fromText
  . Text.words
  . Text.replace "\\\n" " "
  . Text.drop 1 . Text.dropWhile (/= ':')

-- TODO Handle escapes (spaces!) and special characters.
readDepFileDeps :: FilePath -> IO [FilePath]
readDepFileDeps
  = either throwIO (return . parseDepFileDeps)
  . Text.decodeUtf8' <=< FS.readFile

ruleDatabase :: (MonadIO m, Typeable1 m) => RuleDatabase m
ruleDatabase = mconcat
  [ filterFileRule (`hasExtensions` ["c", "o"]) $ \ ofile -> do
    let cfile = "examples/cc" </> Path.filename (Path.dropExtension ofile)
    needFile cfile
    -- FIXME(strager): This doesn't handle cases where
    -- e.g. another stdio.h is created.
    runSystem "cc"
      [ "-c"
      , "-o", Path.encodeString ofile
      , "-MD", "-MD"  -- Generate dep file.
      , Path.encodeString cfile
      ]

    let depfile = ofile `Path.replaceExtension` "d"
    deps <- liftIO $ readDepFileDeps depfile
    needFiles deps

  , oneFileRule (root </> "prog") $ do
    let ofiles = map (root </>) ["a.c.o", "b.c.o"]
    needFiles ofiles
    runSystem "cc"
      $ ["-o", Path.encodeString (root </> "prog")]
      ++ map Path.encodeString ofiles
  ]

root :: FilePath
root = "example-build-dir-cc"

lookupQuestion
  :: (MonadIO m, Typeable1 m)
  => Fingerprint
  -> Maybe (AQuestion m{-undefined-})
lookupQuestion fingerprint = RuleDatabase.lookupQuestion
  (TypeRep fingerprint undefined undefined)
  ruleDatabase

readOracle
  :: (MonadIO m, Typeable1 m)
  => FilePath
  -> IO (OraclePure.State m)
readOracle path = decode `fmap` FS.readFile path
  where
  decode
    = Binary.runGet (OraclePure.getState lookupQuestion)
    . BSLazy.fromStrict

writeOracle :: FilePath -> OraclePure.State m -> IO ()
writeOracle path
  = FS.writeFile path
  . BSLazy.toStrict . Binary.runPut
  . OraclePure.putState

main :: IO ()
main = do
  let dbPath = root </> "build.db"
  exists <- FS.isFile dbPath
  state <- if exists
    then readOracle dbPath
    else return OraclePure.empty

  storage <- newMVar state
  let oracle = InMemory.mkMVarOracleWithStorage storage
  Oracle.recheckAll oracle

  createTree root

  let
    logMessage x = putStrLn ("> " ++ show x)
    run = void . runBuild ruleDatabase oracle logMessage

  run $ buildFile (root </> "prog")

  state' <- takeMVar storage
  writeOracle dbPath state'
