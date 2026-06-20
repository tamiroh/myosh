module Main (main) where

import Myosh.Command
  ( CommandResult (..),
    initialShellState,
    runCommand,
  )

main :: IO ()
main = do
  assertContinue =<< runCommand initialShellState ""
  assertContinue =<< runCommand initialShellState "   "
  assertContinue =<< runCommand initialShellState "cd -"
  assertEqual Stop =<< runCommand initialShellState "exit"

assertContinue :: CommandResult -> IO ()
assertContinue (Continue _) = pure ()
assertContinue Stop = error "Expected Continue, got Stop"

assertEqual :: (Eq a, Show a) => a -> a -> IO ()
assertEqual expected actual =
  if expected == actual
    then pure ()
    else error ("Expected " ++ show expected ++ ", got " ++ show actual)
