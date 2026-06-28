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
import Myosh.Process (ProcessCommand (..), captureCommandOutput, runPipeline, runProcess)
import System.Directory (getCurrentDirectory, getHomeDirectory, setCurrentDirectory)
import System.Exit (ExitCode (..))
import System.IO (hPutStrLn, stderr)
import System.IO.Error (ioeGetErrorString, isDoesNotExistError)

--------------------------------------------------------------------------------
-- Command Model
--------------------------------------------------------------------------------

data Command
  = Empty
  | Simple SimpleCommand
  | Exit
  | ChangeDirectory DirectoryTarget
  | Run FilePath [String]
  | Pipeline [SimpleCommand]
  | Invalid String
  deriving (Eq, Show)

newtype SimpleCommand = SimpleCommand [String]
  deriving (Eq, Show)

data DirectoryTarget
  = HomeDirectory
  | PreviousDirectory
  | DirectoryPath FilePath
  deriving (Eq, Show)

data Token
  = WordToken String
  | PipeToken
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
runCommand state input =
  catch
    ( do
        expanded <- expandCommand (parseCommand input)
        case expanded of
          Left message -> runInvalidCommand state message
          Right command -> executeCommand state command
    )
    (continueAfterInterrupt state)

expandCommand :: Command -> IO (Either String Command)
expandCommand Empty = pure (Right Empty)
expandCommand (Simple command) = expandSimpleCommand command
expandCommand Exit = pure (Right Exit)
expandCommand (ChangeDirectory target) =
  fmap ChangeDirectory <$> expandDirectoryTarget target
expandCommand (Run command arguments) =
  fmap (uncurry Run) <$> expandCommandWords command arguments
expandCommand (Pipeline commands) = do
  expandedCommands <- mapM expandSimplePipelineCommand commands
  pure (Pipeline <$> sequence expandedCommands)
expandCommand (Invalid message) = pure (Right (Invalid message))

expandDirectoryTarget :: DirectoryTarget -> IO (Either String DirectoryTarget)
expandDirectoryTarget HomeDirectory = pure (Right HomeDirectory)
expandDirectoryTarget PreviousDirectory = pure (Right PreviousDirectory)
expandDirectoryTarget (DirectoryPath path) =
  fmap DirectoryPath <$> expandInlineCommands path

expandSimpleCommand :: SimpleCommand -> IO (Either String Command)
expandSimpleCommand (SimpleCommand wordsInCommand) = do
  expandedWords <- mapM expandInlineCommands wordsInCommand
  pure (parseBuiltinCommand =<< sequence expandedWords)

expandSimplePipelineCommand :: SimpleCommand -> IO (Either String SimpleCommand)
expandSimplePipelineCommand (SimpleCommand wordsInCommand) = do
  expandedWords <- mapM expandInlineCommands wordsInCommand
  pure (SimpleCommand <$> sequence expandedWords)

expandCommandWords :: FilePath -> [String] -> IO (Either String (FilePath, [String]))
expandCommandWords command arguments = do
  expandedCommand <- expandInlineCommands command
  expandedArguments <- mapM expandInlineCommands arguments
  pure ((,) <$> expandedCommand <*> sequence expandedArguments)

expandInlineCommands :: String -> IO (Either String String)
expandInlineCommands "" = pure (Right "")
expandInlineCommands ('`' : input) =
  case break (== '`') input of
    (_, "") -> pure (Left "syntax error: unmatched backtick")
    (inlineCommand, _ : rest) -> do
      case parseCommandSubstitution inlineCommand of
        Left message -> pure (Left message)
        Right processCommand -> do
          expanded <- runCommandSubstitution processCommand
          remaining <- expandInlineCommands rest
          case remaining of
            Left message -> pure (Left message)
            Right commandLine -> pure (Right (expanded ++ commandLine))
expandInlineCommands (char : input) = do
  remaining <- expandInlineCommands input
  case remaining of
    Left message -> pure (Left message)
    Right commandLine -> pure (Right (char : commandLine))

runCommandSubstitution :: ProcessCommand -> IO String
runCommandSubstitution processCommand@(ProcessCommand command _) =
  catch
    ( do
        (exitCode, standardOutput) <- captureCommandOutput processCommand
        reportExitCode exitCode
        pure standardOutput
    )
    ( \err -> do
        reportError command err
        pure ""
    )

parseCommandSubstitution :: String -> Either String ProcessCommand
parseCommandSubstitution input =
  Right (processCommandFromWords (words input))

parseCommand :: String -> Command
parseCommand input =
  case parseTokens (lexCommandLine input) of
    Left message -> Invalid message
    Right command -> command

lexCommandLine :: String -> [Token]
lexCommandLine =
  lexTokens [] Nothing

lexTokens :: [Token] -> Maybe String -> String -> [Token]
lexTokens tokens Nothing "" = reverse tokens
lexTokens tokens (Just word) "" = reverse (lexWord word : tokens)
lexTokens tokens Nothing (char : input)
  | isShellSpace char = lexTokens tokens Nothing input
  | char == '`' = lexTokens tokens (Just (readBacktickWord input)) (dropBacktickWord input)
  | otherwise = lexTokens tokens (Just [char]) input
lexTokens tokens (Just word) (char : input)
  | isShellSpace char = lexTokens (lexWord word : tokens) Nothing input
  | char == '`' =
      lexTokens tokens (Just (word ++ readBacktickWord input)) (dropBacktickWord input)
  | otherwise = lexTokens tokens (Just (word ++ [char])) input

readBacktickWord :: String -> String
readBacktickWord input =
  case break (== '`') input of
    (inlineCommand, "") -> '`' : inlineCommand
    (inlineCommand, _) -> '`' : inlineCommand ++ "`"

dropBacktickWord :: String -> String
dropBacktickWord input =
  case break (== '`') input of
    (_, "") -> ""
    (_, _ : rest) -> rest

isShellSpace :: Char -> Bool
isShellSpace char = char `elem` [' ', '\t', '\n']

lexWord :: String -> Token
lexWord "|" = PipeToken
lexWord word = WordToken word

parseTokens :: [Token] -> Either String Command
parseTokens tokens =
  case splitPipeline tokens of
    [] -> Right Empty
    [commandTokens] -> Simple <$> parseSimpleCommand commandTokens
    tokensByCommand ->
      if any null tokensByCommand
        then Left "syntax error near unexpected token `|'"
        else Pipeline <$> mapM parseSimpleCommand tokensByCommand

parseBuiltinCommand :: [String] -> Either String Command
parseBuiltinCommand tokens =
  case tokens of
    [] -> Right Empty
    ["exit"] -> Right Exit
    ["cd"] -> Right (ChangeDirectory HomeDirectory)
    ["cd", "-"] -> Right (ChangeDirectory PreviousDirectory)
    ["cd", path] -> Right (ChangeDirectory (DirectoryPath path))
    "cd" : _ -> Right (Run "cd" [])
    command : arguments -> Right (Run command arguments)

parseSimpleCommand :: [Token] -> Either String SimpleCommand
parseSimpleCommand tokens =
  SimpleCommand <$> wordTokens tokens

processCommandFromWords :: [String] -> ProcessCommand
processCommandFromWords [] = ProcessCommand "" []
processCommandFromWords (command : arguments) = ProcessCommand command arguments

wordTokens :: [Token] -> Either String [String]
wordTokens =
  mapM wordToken

wordToken :: Token -> Either String String
wordToken (WordToken word) = Right word
wordToken PipeToken = Left "syntax error near unexpected token `|'"

splitPipeline :: [Token] -> [[Token]]
splitPipeline =
  foldr splitToken [[]]

splitToken :: Token -> [[Token]] -> [[Token]]
splitToken PipeToken commands = [] : commands
splitToken token [] = [[token]]
splitToken token (command : commands) = (token : command) : commands

--------------------------------------------------------------------------------
-- Command Dispatch
--------------------------------------------------------------------------------

executeCommand :: ShellState -> Command -> IO CommandResult
executeCommand state Empty = runEmptyCommand state
executeCommand state (Simple (SimpleCommand wordsInCommand)) =
  case parseBuiltinCommand wordsInCommand of
    Left message -> runInvalidCommand state message
    Right command -> executeCommand state command
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

runPipelineCommand :: ShellState -> [SimpleCommand] -> IO CommandResult
runPipelineCommand state commands =
  catch
    ( catch
        ( runPipeline (map processCommandFromSimpleCommand commands)
            >>= mapM_ reportExitCode
            >> pure (Continue state)
        )
        (continueAfterError state "pipeline")
    )
    (continueAfterInterrupt state)

processCommandFromSimpleCommand :: SimpleCommand -> ProcessCommand
processCommandFromSimpleCommand (SimpleCommand wordsInCommand) =
  processCommandFromWords wordsInCommand

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
