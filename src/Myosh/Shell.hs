module Myosh.Shell
  ( runShell,
  )
where

import Myosh.Command (CommandResult (..), runCommand)
import Control.Monad.IO.Class (liftIO)
import System.Console.Haskeline
  ( InputT,
    defaultSettings,
    getInputLine,
    runInputT,
  )

runShell :: IO ()
runShell = runInputT defaultSettings shellLoop

shellLoop :: InputT IO ()
shellLoop = do
  input <- getInputLine "myosh> "
  case input of
    Nothing -> pure ()
    Just line -> do
      result <- liftIO (runCommand line)
      case result of
        Continue -> shellLoop
        Stop -> pure ()
