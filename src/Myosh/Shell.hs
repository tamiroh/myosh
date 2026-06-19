module Myosh.Shell
  ( runShell,
  )
where

import Myosh.Command (CommandResult (..), runCommand)
import System.IO (hFlush, isEOF, stdout)

runShell :: IO ()
runShell = do
  putStr "myosh> "
  hFlush stdout
  done <- isEOF
  if done
    then putStrLn ""
    else do
      result <- getLine >>= runCommand
      case result of
        Continue -> runShell
        Stop -> pure ()
