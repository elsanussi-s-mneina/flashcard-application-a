module Main where

import Lib (welcome)
import Lesson
import System.IO
import System.Exit


-- | list of flashcards for running the program
flashcards :: [Flashcard]
flashcards = [flashcard1, flashcard2]

flashcard1 :: Flashcard
flashcard1 = Flashcard {front = "the", back = "le/la"}

flashcard2 :: Flashcard
flashcard2 = Flashcard {front = "a", back = "un/une"}

-- | This program runs in the terminal. It outputs
-- text to the student.
main :: IO ()
main =
  do
  welcome     -- show a welcome message.
  commandLineLoop

commandLineLoop :: IO ()
commandLineLoop =
  do
  putStrLn "Enter 'a' to show both front and back of each card."
  putStrLn "Enter 'f' to show the front of each card."
  putStrLn "Enter 'b' to show the back of each card."
  putStrLn "Enter 'x' to exit the application."
  putStrLn "" -- blank line
  putStr  "> " -- terminal prompt to show the user
  hFlush stdout  -- We need to flush standard out
                 -- so that the terminal prompt appears
                 -- before the user input rather than after.
  userInput <- getLine
  if userInput == "a"
  then
    do
    putStrLn "Printing Lesson summary:"
    putStrLn (lessonSummary flashcards)
  else
    if userInput == "f"
    then
      do
      putStrLn "Print only fronts of each card:"
      putStrLn (frontSummary flashcards)
    else
      if userInput == "b"
      then
        do
        putStrLn "Print only backs of each card:"
        putStrLn (backSummary flashcards)
      else
        if userInput == "x"
        then
          exitSuccess
        else
          putStrLn ("Unrecognized input: (" ++ userInput ++ ")")
  commandLineLoop  -- loop (go back to the beginning)
