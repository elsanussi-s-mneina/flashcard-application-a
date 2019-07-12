module Main where

import Lesson
import System.IO
import System.Exit


-- | This program runs in the terminal. It outputs
-- text to the student.
main :: IO ()
main =
  do
  putStrLn "Welcome to Remember the Letter (Haskell)"
  putStrLn ""
  putStrLn "Enter 'open' if you want to open a lesson file"
  putStrLn "Enter 'n' if you want to create a new lesson."
  putStrLn ""
  putStr "> "
  hFlush stdout

  userInput <- getLine
  case
    userInput
    of "open"  ->
               do
               putStrLn "Enter a name for a file to open:"
               putStr  "> "
               hFlush stdout
               fileName <- getLine
               fileContents <- readFile fileName
               flashcards' <- return (tabSeparatedValuesToLesson fileContents)
               commandLineLoop flashcards'
               return ()
       _       ->
               do
               commandLineLoop []


commandLineLoop :: [Flashcard] -> IO ()
commandLineLoop flashcards =
  do
  putStrLn "Enter 'a' to show both front and back of each card."
  putStrLn "Enter 'f' to show the front of each card."
  putStrLn "Enter 'b' to show the back of each card."
  putStrLn "Enter 'save' to save all flashcards"
  putStrLn "Enter 'x' to exit the application."
  putStrLn "" -- blank line
  putStr  "> " -- terminal prompt to show the user
  hFlush stdout  -- We need to flush standard out
                 -- so that the terminal prompt appears
                 -- before the user input rather than after.
  userInput <- getLine

  case
    userInput
    of "a"    ->
              do
              putStrLn "Printing Lesson summary:"
              putStrLn (lessonSummary flashcards)
       "f"    ->
              do
              putStrLn "Print only fronts of each card:"
              putStrLn (frontSummary flashcards)
       "b"    ->
              do
              putStrLn "Print only backs of each card:"
              putStrLn (backSummary flashcards)
       "save" ->
              do
              -- Let the user choose the file name.
              putStrLn "Enter a name for a file to save to:"
              putStr  "> "
              hFlush stdout
              fileName <- getLine

              putStrLn ("Saving flashcards to file called '" ++ fileName ++ "'")
              _ <- writeFile fileName (tabSeparatedValuesOfLesson flashcards)
              putStrLn ("Done writing to file named " ++ fileName)
       "x"    ->  exitSuccess
       _      ->  putStrLn ("Unrecognized input: (" ++ userInput ++ ")")
  commandLineLoop flashcards -- loop (go back to the beginning)
