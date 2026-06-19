module Main (main) where

import Myosh.Command
  ( CommandResult (..),
    runCommand,
  )

main :: IO ()
main = do
  assertEqual Continue =<< runCommand ""
  assertEqual Continue =<< runCommand "   "
  assertEqual Stop =<< runCommand "exit"

assertEqual :: (Eq a, Show a) => a -> a -> IO ()
assertEqual expected actual =
  if expected == actual
    then pure ()
    else error ("Expected " ++ show expected ++ ", got " ++ show actual)
