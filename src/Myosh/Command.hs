module Myosh.Command
  ( CommandResult (..),
    runCommand,
  )
where

import Control.Exception (IOException, catch)
import System.Directory (setCurrentDirectory)
import System.Exit (ExitCode (..))
import System.IO (hPutStrLn, stderr)
import System.Process (rawSystem)

data Command
  = Empty
  | Exit
  | ChangeDirectory FilePath
  | Run FilePath [String]
  deriving (Eq, Show)

data CommandResult
  = Continue
  | Stop
  deriving (Eq, Show)

runCommand :: String -> IO CommandResult
runCommand input = executeCommand (parseCommand input)

parseCommand :: String -> Command
parseCommand input =
  case words input of
    [] -> Empty
    ["exit"] -> Exit
    ["cd"] -> ChangeDirectory "."
    ["cd", path] -> ChangeDirectory path
    "cd" : _ -> Run "cd" []
    command : arguments -> Run command arguments

executeCommand :: Command -> IO CommandResult
executeCommand Empty = runEmptyCommand
executeCommand Exit = runExitCommand
executeCommand (ChangeDirectory path) = runChangeDirectoryCommand path
executeCommand (Run command arguments) = runExternalCommand command arguments

runEmptyCommand :: IO CommandResult
runEmptyCommand = pure Continue

runExitCommand :: IO CommandResult
runExitCommand = pure Stop

runChangeDirectoryCommand :: FilePath -> IO CommandResult
runChangeDirectoryCommand path =
  catch
    (setCurrentDirectory path >> pure Continue)
    (reportError path)

runExternalCommand :: FilePath -> [String] -> IO CommandResult
runExternalCommand command arguments =
  catch
    (rawSystem command arguments >>= reportExitCode >> pure Continue)
    (reportError command)

reportExitCode :: ExitCode -> IO ()
reportExitCode ExitSuccess = pure ()
reportExitCode (ExitFailure code) =
  hPutStrLn stderr ("Exited with status " ++ show code)

reportError :: String -> IOException -> IO CommandResult
reportError context err = do
  hPutStrLn stderr (context ++ ": " ++ show err)
  pure Continue
