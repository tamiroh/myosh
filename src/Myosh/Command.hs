{-# LANGUAGE OverloadedRecordDot #-}

module Myosh.Command
  ( CommandResult (..),
    ShellState (..),
    initialShellState,
    runCommand,
  )
where

import Control.Exception
  ( AsyncException (UserInterrupt),
    IOException,
    catch,
    throwIO,
  )
import System.Directory (getCurrentDirectory, getHomeDirectory, setCurrentDirectory)
import System.Exit (ExitCode (..))
import System.IO (hPutStrLn, stderr)
import System.IO.Error (ioeGetErrorString, isDoesNotExistError)
import System.Process (rawSystem)

data Command
  = Empty
  | Exit
  | ChangeDirectory DirectoryTarget
  | Run FilePath [String]
  deriving (Eq, Show)

data DirectoryTarget
  = HomeDirectory
  | PreviousDirectory
  | DirectoryPath FilePath
  deriving (Eq, Show)

newtype ShellState = ShellState
  { previousDirectory :: Maybe FilePath
  }
  deriving (Eq, Show)

data CommandResult
  = Continue ShellState
  | Stop
  deriving (Eq, Show)

initialShellState :: ShellState
initialShellState = ShellState {previousDirectory = Nothing}

runCommand :: ShellState -> String -> IO CommandResult
runCommand state input = executeCommand state (parseCommand input)

parseCommand :: String -> Command
parseCommand input =
  case words input of
    [] -> Empty
    ["exit"] -> Exit
    ["cd"] -> ChangeDirectory HomeDirectory
    ["cd", "-"] -> ChangeDirectory PreviousDirectory
    ["cd", path] -> ChangeDirectory (DirectoryPath path)
    "cd" : _ -> Run "cd" []
    command : arguments -> Run command arguments

executeCommand :: ShellState -> Command -> IO CommandResult
executeCommand state Empty = runEmptyCommand state
executeCommand _ Exit = runExitCommand
executeCommand state (ChangeDirectory target) =
  runChangeDirectoryCommand state target
executeCommand state (Run command arguments) =
  runExternalCommand state command arguments

runEmptyCommand :: ShellState -> IO CommandResult
runEmptyCommand state = pure (Continue state)

runExitCommand :: IO CommandResult
runExitCommand = pure Stop

runChangeDirectoryCommand :: ShellState -> DirectoryTarget -> IO CommandResult
runChangeDirectoryCommand state target =
  case target of
    PreviousDirectory ->
      case state.previousDirectory of
        Nothing -> do
          hPutStrLn stderr "cd: OLDPWD not set"
          pure (Continue state)
        Just path -> changeDirectory state path True
    HomeDirectory ->
      getHomeDirectory >>= \path -> changeDirectory state path False
    DirectoryPath path ->
      changeDirectory state path False

runExternalCommand :: ShellState -> FilePath -> [String] -> IO CommandResult
runExternalCommand state command arguments =
  catch
    ( catch
        (rawSystem command arguments >>= reportExitCode >> pure (Continue state))
        (continueAfterError state command)
    )
    (continueAfterInterrupt state)

changeDirectory :: ShellState -> FilePath -> Bool -> IO CommandResult
changeDirectory state path printPath =
  catch
    ( do
        currentDirectory <- getCurrentDirectory
        setCurrentDirectory path
        if printPath
          then putStrLn path
          else pure ()
        pure (Continue state {previousDirectory = Just currentDirectory})
    )
    (continueAfterError state path)

reportExitCode :: ExitCode -> IO ()
reportExitCode ExitSuccess = pure ()
reportExitCode (ExitFailure code) =
  hPutStrLn stderr ("Exited with status " ++ show code)

continueAfterError :: ShellState -> String -> IOException -> IO CommandResult
continueAfterError state context err = do
  reportError context err
  pure (Continue state)

continueAfterInterrupt :: ShellState -> AsyncException -> IO CommandResult
continueAfterInterrupt state UserInterrupt = pure (Continue state)
continueAfterInterrupt _ err = throwIO err

reportError :: String -> IOException -> IO ()
reportError context err =
  hPutStrLn stderr (context ++ ": " ++ formatError err)

formatError :: IOException -> String
formatError err =
  if isDoesNotExistError err
    then "command not found"
    else ioeGetErrorString err
