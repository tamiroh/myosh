module Myosh.Shell
  ( runShell,
  )
where

import Myosh.Command
  ( CommandResult (..),
    ShellState,
    initialShellState,
    runCommand,
  )
import Control.Monad.IO.Class (liftIO)
import System.Console.Haskeline
  ( InputT,
    defaultSettings,
    getInputLine,
    handleInterrupt,
    runInputT,
    withInterrupt,
  )

runShell :: IO ()
runShell = runInputT defaultSettings (withInterrupt (shellLoop initialShellState))

shellLoop :: ShellState -> InputT IO ()
shellLoop state =
  handleInterrupt (shellLoop state) $ do
    input <- getInputLine "myosh> "
    case input of
      Nothing -> pure ()
      Just line -> do
        result <- liftIO (runCommand state line)
        case result of
          Continue nextState -> shellLoop nextState
          Stop -> pure ()
