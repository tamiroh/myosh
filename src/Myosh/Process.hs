module Myosh.Process
  ( ProcessCommand (..),
    captureCommandOutput,
    captureProcess,
    runPipeline,
    runProcess,
  )
where

import System.Exit (ExitCode (..))
import System.IO (Handle, hClose, hPutStr, stderr)
import System.Process
  ( ProcessHandle,
    StdStream (..),
    createProcess,
    proc,
    rawSystem,
    readCreateProcessWithExitCode,
    std_in,
    std_out,
    waitForProcess,
  )

data ProcessCommand = ProcessCommand FilePath [String]
  deriving (Eq, Show)

runProcess :: ProcessCommand -> IO ExitCode
runProcess (ProcessCommand command arguments) =
  rawSystem command arguments

captureProcess :: ProcessCommand -> IO (ExitCode, String, String)
captureProcess (ProcessCommand command arguments) =
  readCreateProcessWithExitCode (proc command arguments) ""

captureCommandOutput :: ProcessCommand -> IO (ExitCode, String)
captureCommandOutput (ProcessCommand "" []) = pure (ExitSuccess, "")
captureCommandOutput processCommand = do
  (exitCode, standardOutput, standardError) <- captureProcess processCommand
  hPutStr stderr standardError
  pure (exitCode, trimTrailingNewlines standardOutput)

trimTrailingNewlines :: String -> String
trimTrailingNewlines =
  reverse . dropWhile (== '\n') . reverse

runPipeline :: [ProcessCommand] -> IO [ExitCode]
runPipeline commands =
  startPipeline Nothing commands >>= mapM waitForProcess

startPipeline :: Maybe Handle -> [ProcessCommand] -> IO [ProcessHandle]
startPipeline _ [] = pure []
startPipeline inputHandle [ProcessCommand command arguments] = do
  (_, _, _, processHandle) <-
    createProcess
      (proc command arguments)
        { std_in = maybe Inherit UseHandle inputHandle
        }
  closeInputHandle inputHandle
  pure [processHandle]
startPipeline inputHandle (ProcessCommand command arguments : commands) = do
  (_, outputHandle, _, processHandle) <-
    createProcess
      (proc command arguments)
        { std_in = maybe Inherit UseHandle inputHandle,
          std_out = CreatePipe
        }
  closeInputHandle inputHandle
  case outputHandle of
    Nothing -> pure [processHandle]
    Just handle -> (processHandle :) <$> startPipeline (Just handle) commands

closeInputHandle :: Maybe Handle -> IO ()
closeInputHandle Nothing = pure ()
closeInputHandle (Just handle) = hClose handle
