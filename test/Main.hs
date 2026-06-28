module Main (main) where

import Control.Exception (IOException, catch, finally)
import Myosh.Command
  ( CommandResult (..),
    initialShellState,
    runCommand,
  )
import System.Directory
  ( createDirectoryIfMissing,
    doesFileExist,
    getCurrentDirectory,
    getTemporaryDirectory,
    removePathForcibly,
    setCurrentDirectory,
  )

main :: IO ()
main = do
  assertContinue =<< runCommand initialShellState ""
  assertContinue =<< runCommand initialShellState "   "
  assertContinue =<< runCommand initialShellState "cd -"
  assertEqual Stop =<< runCommand initialShellState "exit"
  withinTempDirectory "myosh-parser-test" $ do
    currentDirectory <- getCurrentDirectory
    assertContinue =<< runCommand initialShellState "cd extra arguments"
    assertEqual currentDirectory =<< getCurrentDirectory
    assertContinue =<< runCommand initialShellState "printf pipeline-output | xargs touch"
    assertBool =<< doesFileExist "pipeline-output"
    assertContinue =<< runCommand initialShellState "touch `printf expanded-file`"
    assertBool =<< doesFileExist "expanded-file"
    assertContinue =<< runCommand initialShellState "touch before-pipe `echo |` after-pipe"
    assertBool =<< doesFileExist "before-pipe"
    assertBool =<< doesFileExist "|"
    assertBool =<< doesFileExist "after-pipe"

assertContinue :: CommandResult -> IO ()
assertContinue (Continue _) = pure ()
assertContinue Stop = error "Expected Continue, got Stop"

assertEqual :: (Eq a, Show a) => a -> a -> IO ()
assertEqual expected actual =
  if expected == actual
    then pure ()
    else error ("Expected " ++ show expected ++ ", got " ++ show actual)

assertBool :: Bool -> IO ()
assertBool True = pure ()
assertBool False = error "Expected True, got False"

withinTempDirectory :: FilePath -> IO a -> IO a
withinTempDirectory name action = do
  root <- getTemporaryDirectory
  currentDirectory <- getCurrentDirectory
  let path = root ++ "/" ++ name
  removePathIfExists path
  createDirectoryIfMissing True path
  setCurrentDirectory path
  action
    `finally` do
      setCurrentDirectory currentDirectory
      removePathIfExists path

removePathIfExists :: FilePath -> IO ()
removePathIfExists path =
  removePathForcibly path `catch` ignoreIOException

ignoreIOException :: IOException -> IO ()
ignoreIOException _ = pure ()
