module Main where

import Lib (welcome)
import Lesson (lessonSummary)

main :: IO ()
main =
  do
  welcome
  putStrLn ""
  putStrLn "Printing Lesson summary:"
  putStrLn lessonSummary
