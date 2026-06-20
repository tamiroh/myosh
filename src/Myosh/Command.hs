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
import Myosh.Process (ProcessCommand (..), runPipeline, runProcess)
import System.Directory (getCurrentDirectory, getHomeDirectory, setCurrentDirectory)
import System.Exit (ExitCode (..))
import System.IO (hPutStrLn, stderr)
import System.IO.Error (ioeGetErrorString, isDoesNotExistError)

--------------------------------------------------------------------------------
-- Command Model
--------------------------------------------------------------------------------

data Command
  = Empty
  | Exit
  | ChangeDirectory DirectoryTarget
  | Run FilePath [String]
  | Pipeline [ProcessCommand]
  | Invalid String
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

--------------------------------------------------------------------------------
-- Command Line Parsing
--------------------------------------------------------------------------------

runCommand :: ShellState -> String -> IO CommandResult
runCommand state input = executeCommand state (parseCommand input)

parseCommand :: String -> Command
parseCommand input =
  case splitPipeline (words input) of
    [] -> Empty
    [tokens] -> parseBuiltinCommand tokens
    tokensByCommand ->
      if any null tokensByCommand
        then Invalid "syntax error near unexpected token `|'"
        else Pipeline (map parseProcessCommand tokensByCommand)

parseBuiltinCommand :: [String] -> Command
parseBuiltinCommand tokens =
  case tokens of
    [] -> Empty
    ["exit"] -> Exit
    ["cd"] -> ChangeDirectory HomeDirectory
    ["cd", "-"] -> ChangeDirectory PreviousDirectory
    ["cd", path] -> ChangeDirectory (DirectoryPath path)
    "cd" : _ -> Run "cd" []
    command : arguments -> Run command arguments

parseProcessCommand :: [String] -> ProcessCommand
parseProcessCommand [] = ProcessCommand "" []
parseProcessCommand (command : arguments) = ProcessCommand command arguments

splitPipeline :: [String] -> [[String]]
splitPipeline =
  foldr splitToken [[]]

splitToken :: String -> [[String]] -> [[String]]
splitToken "|" commands = [] : commands
splitToken token [] = [[token]]
splitToken token (command : commands) = (token : command) : commands

--------------------------------------------------------------------------------
-- Command Dispatch
--------------------------------------------------------------------------------

executeCommand :: ShellState -> Command -> IO CommandResult
executeCommand state Empty = runEmptyCommand state
executeCommand _ Exit = runExitCommand
executeCommand state (ChangeDirectory target) =
  runChangeDirectoryCommand state target
executeCommand state (Run command arguments) =
  runExternalCommand state command arguments
executeCommand state (Pipeline commands) = runPipelineCommand state commands
executeCommand state (Invalid message) = runInvalidCommand state message

--------------------------------------------------------------------------------
-- Command / Empty
--------------------------------------------------------------------------------

runEmptyCommand :: ShellState -> IO CommandResult
runEmptyCommand state = pure (Continue state)

--------------------------------------------------------------------------------
-- Command / Exit
--------------------------------------------------------------------------------

runExitCommand :: IO CommandResult
runExitCommand = pure Stop

--------------------------------------------------------------------------------
-- Command / Invalid
--------------------------------------------------------------------------------

runInvalidCommand :: ShellState -> String -> IO CommandResult
runInvalidCommand state message = do
  hPutStrLn stderr message
  pure (Continue state)

--------------------------------------------------------------------------------
-- Command / Change Directory
--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------
-- Command / External Process
--------------------------------------------------------------------------------

runExternalCommand :: ShellState -> FilePath -> [String] -> IO CommandResult
runExternalCommand state command arguments =
  catch
    ( catch
        (runProcess (ProcessCommand command arguments) >>= reportExitCode >> pure (Continue state))
        (continueAfterError state command)
    )
    (continueAfterInterrupt state)

--------------------------------------------------------------------------------
-- Command / Pipeline
--------------------------------------------------------------------------------

runPipelineCommand :: ShellState -> [ProcessCommand] -> IO CommandResult
runPipelineCommand state commands =
  catch
    ( catch
        (runPipeline commands >>= mapM_ reportExitCode >> pure (Continue state))
        (continueAfterError state "pipeline")
    )
    (continueAfterInterrupt state)

--------------------------------------------------------------------------------
-- Exit Status Reporting
--------------------------------------------------------------------------------

reportExitCode :: ExitCode -> IO ()
reportExitCode ExitSuccess = pure ()
reportExitCode (ExitFailure code) =
  hPutStrLn stderr ("Exited with status " ++ show code)

--------------------------------------------------------------------------------
-- Error Handling
--------------------------------------------------------------------------------

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
