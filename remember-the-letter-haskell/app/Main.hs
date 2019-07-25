module Main (main) where

import Prelude ((++), getLine, IO, putStr, putStrLn, return)

import Lesson (addFlashcardToLesson, backSummary, Flashcard,  frontSummary,
               lessonSummary, tabSeparatedValuesOfLesson,
               tabSeparatedValuesToLesson)
import System.IO (hFlush, readFile, stdout, writeFile)
import System.Exit (exitSuccess)


-- | This program runs in the terminal. It outputs
-- text to the student.
main :: IO ()
main =
  do
  putStrLn "Welcome to Remember the Letter (Haskell)"
  putStrLn ""
  putStrLn "Enter 'open' if you want to open a lesson file"
  putStrLn "Enter 'n' if you want to create a new lesson.\n"
  printPrompt

  userInput <- getLine
  case
    userInput
    of "open"  ->
               do
               putStrLn "Enter a name for a file to open:"
               printPrompt
               fileName <- getLine
               fileContents <- readFile fileName
               let flashcards' = tabSeparatedValuesToLesson fileContents
               commandLineLoop flashcards'
               return ()
       _       ->
               commandLineLoop []


commandLineLoop :: [Flashcard] -> IO ()
commandLineLoop flashcards =
  do
  putStrLn "Enter 'a' to show both front and back of each card."
  putStrLn "Enter 'f' to show the front of each card."
  putStrLn "Enter 'b' to show the back of each card."
  putStrLn "Enter 'add' to add a flashcard."
  putStrLn "Enter 'save' to save all flashcards"
  putStrLn "Enter 'x' to exit the application."
  putStrLn "" -- blank line
  printPrompt
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
       "add"  ->
              do
              putStrLn "Adding a flashcard..."
              putStrLn "Enter the front side:"
              printPrompt
              fSide <- getLine
              putStrLn ("You entered the following for the front side: (" ++
                         fSide ++ ")")
              putStrLn "Enter the back side:"
              printPrompt
              bSide  <- getLine
              putStrLn ("You entered the following for the back side: (" ++
                         bSide ++ ")")
              flashcards' <- return (addFlashcardToLesson
                                       flashcards fSide bSide)
              putStrLn "Done adding flashcard."
              commandLineLoop flashcards' -- loop (go back to the beginning)
       "save" ->
              do
              -- Let the user choose the file name.
              putStrLn "Enter a name for a file to save to:"
              printPrompt
              fileName <- getLine

              putStrLn ("Saving flashcards to file called '" ++ fileName ++ "'")
              _ <- writeFile fileName (tabSeparatedValuesOfLesson flashcards)
              putStrLn ("Done writing to file named " ++ fileName)
       "x"    ->  exitSuccess
       _      ->  putStrLn ("Unrecognized input: (" ++ userInput ++ ")")
  commandLineLoop flashcards -- loop (go back to the beginning)

printPrompt :: IO ()
printPrompt =
  do
  putStr  "> "  -- terminal prompt to show the user
  hFlush stdout -- We need to flush standard out
                -- so that the terminal prompt appears
                -- before the user input rather than after.
