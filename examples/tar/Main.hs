-- | Port of Shake's Examples.Tar.Main to B.

import Control.Exception (throwIO)
import Control.Monad
import Control.Monad.IO.Class
import Data.Semigroup
import System.Directory (createDirectoryIfMissing)
import System.Exit
import System.FilePath ((</>))
import System.Process (rawSystem)

import B.File
import B.Monad
import B.RuleDatabase (RuleDatabase)

import qualified B.Oracle.InMemory as InMemory

readFileLines :: FilePath -> BuildRule IO [String]
readFileLines path = do
  needFiles [path]
  liftM lines . liftIO $ readFile path

ruleDatabase :: RuleDatabase IO
ruleDatabase = mconcat
  [ fileRule (root </> "result.tar") $ do
    files <- readFileLines "examples/tar/list.txt"
    needFiles files
    exit <- liftIO . rawSystem "tar"
      $ ["-cf", root </> "result.tar"] ++ files
    case exit of
      ExitSuccess -> return ()
      _ -> liftIO $ throwIO exit
  ]

root :: FilePath
root = "example-build-dir"

main :: IO ()
main = do
  oracle <- InMemory.mkSTMOracle
  createDirectoryIfMissing True root

  let
    logMessage x = putStrLn ("> " ++ show x)
    run = print <=< runBuild ruleDatabase oracle logMessage

  run $ buildFile (root </> "result.tar")
