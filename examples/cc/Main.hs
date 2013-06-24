{-# LANGUAGE OverloadedStrings #-}

import Control.Exception (throwIO)
import Control.Monad
import Control.Monad.IO.Class
import Data.Semigroup
import Data.Text (Text)
import Data.Typeable
import Filesystem (createTree)
import Filesystem.Path.CurrentOS (FilePath, (</>))
import Prelude hiding (FilePath, readFile)
import System.Exit
import System.Process (rawSystem)

import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Filesystem.Path.CurrentOS as Path

import B.File
import B.Monad
import B.RuleDatabase (RuleDatabase)

import qualified B.Oracle.InMemory as InMemory

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
readDepFileDeps
  :: (MonadIO m, Typeable1 m)
  => FilePath
  -> BuildRule m [FilePath]
readDepFileDeps
  = either (liftIO . throwIO) (return . parseDepFileDeps)
  . Text.decodeUtf8' <=< readFile

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
    deps <- readDepFileDeps depfile
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

main :: IO ()
main = do
  oracle <- InMemory.mkSTMOracle
  createTree root

  let
    logMessage x = putStrLn ("> " ++ show x)
    run = print <=< runBuild ruleDatabase oracle logMessage

  run $ buildFile (root </> "prog")
