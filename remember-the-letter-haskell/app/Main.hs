{-|
Module      : Main
Description : This module starts the program. The program runs on the terminal. The program shows text,
and waits for user input.

All input and output happen in this module.
-}
module Main (main) where

import Prelude (IO, ($), (++), (==), getLine, head, putStr, putStrLn, return, tail, null)

import qualified Lesson
import Flashcard (Flashcard)
import qualified Flashcard
import System.IO (hFlush, readFile, stdout, writeFile)
import System.Exit (exitSuccess)
import Control.Monad (unless)
import qualified LanguageUserInterface as UIText


import LanguageIDs (TwoLetterLanguageID(EN))

lang :: TwoLetterLanguageID
lang = EN

-- | This program runs in the terminal. It outputs
-- text to the student.
main :: IO ()
main =
  do
  putStrLn $ UIText.welcomeToProgram lang
  putStrLn ""
  putStrLn $ UIText.openLessonFileMenuItem lang
  putStrLn $ UIText.createLessonFileMenuItem lang
  putStrLn $ UIText.exitMenuItem lang
  printPrompt

  userInput <- getLine
  case userInput of
        _ | userInput == UIText.openLessonFileMenuItemShortcut lang
                 ->
                 do
                 putStrLn $ UIText.fileNamePromptAtFileOpening lang
                 printPrompt
                 fileName <- getLine
                 fileContents <- readFile fileName
                 let flashcards' = Lesson.fromTabSeparatedValues fileContents
                 commandLineLoop flashcards'
                 return ()
        _ | userInput == UIText.createLessonFileMenuItemShortcut lang
                 ->
                 commandLineLoop []
        _ | userInput == UIText.exitMenuItemShortcut lang ->  exitSuccess
        _
                 ->
                 do
                 putStrLn $ UIText.unrecognizedInputMessage lang ++ " \"" ++ userInput ++  "\""
                 main

commandLineLoop :: [Flashcard] -> IO ()
commandLineLoop flashcards =
  do
  putStrLn $ UIText.showFrontAndBackMenuItem lang
  putStrLn $ UIText.showFrontMenuItem lang
  putStrLn $ UIText.showBackMenuItem lang
  putStrLn $ UIText.addMenuItem lang
  putStrLn $ UIText.saveMenuItem lang

  unless (null flashcards) (putStrLn $ UIText.startQuizMenuItem lang)

  putStrLn $ UIText.exitMenuItem lang
  putStrLn "" -- blank line
  printPrompt
  userInput <- getLine

  case userInput of
       _ | userInput == UIText.showFrontAndBackMenuItemShortcut lang
              ->
              do
              putStrLn $ UIText.printingLessonSummaryHeader lang
              putStrLn (Lesson.summary flashcards)
       _ | userInput == UIText.showFrontMenuItemShortcut lang
              ->
              do
              putStrLn $ UIText.printFrontsHeader lang
              putStrLn (Lesson.frontSummary flashcards)
       _ | userInput == UIText.showBackMenuItemShortcut lang
              ->
              do
              putStrLn $ UIText.printBacksHeader lang
              putStrLn (Lesson.backSummary flashcards)
       _ | userInput == UIText.addMenuItemShortcut lang
              ->
              do
              putStrLn $ UIText.addingFlashcardHeader lang
              putStrLn $ UIText.enterFrontSidePrompt lang
              printPrompt
              fSide <- getLine
              putStrLn (UIText.enterFrontSideAcknowledgement lang ++ " (" ++
                         fSide ++ ")")
              putStrLn $ UIText.enterBackSidePrompt lang
              printPrompt
              bSide  <- getLine
              putStrLn (UIText.enterBackSideAcknowledgement lang ++ " (" ++
                         bSide ++ ")")
              let flashcards' = Lesson.addFlashcard flashcards fSide bSide
              putStrLn $ UIText.doneAddingFlashcardMessage lang
              commandLineLoop flashcards' -- loop (go back to the beginning)
       _ | userInput == UIText.startQuizMenuItemShortcut lang
              ->
              do
              quizShowingFrontExpectingBack flashcards
       _ | userInput == UIText.saveMenuItemShortcut lang
              ->
              do
              -- Let the user choose the file name.
              putStrLn $ UIText.nameOfFileToSavePrompt lang
              printPrompt
              fileName <- getLine

              putStrLn (UIText.savingFlashcardsMessage lang ++  "'" ++ fileName ++ "'")
              _ <- writeFile fileName (Lesson.toTabSeparatedValues flashcards)
              putStrLn (UIText.doneSavingFileMessage lang ++ " " ++ fileName)
       _ | userInput == UIText.exitMenuItemShortcut lang ->  exitSuccess
       _      ->  putStrLn (UIText.unrecognizedInputMessage lang ++ " (" ++ userInput ++ ")")
  commandLineLoop flashcards -- loop (go back to the beginning)

printPrompt :: IO ()
printPrompt =
  do
  putStr  "> "  -- terminal prompt to show the user
  hFlush stdout -- We need to flush standard out
                -- so that the terminal prompt appears
                -- before the user input rather than after.

-- | Asks user to remember the back side of a flashcard .
quizShowingFrontExpectingBack :: [Flashcard] -> IO ()
quizShowingFrontExpectingBack [] = return ()
quizShowingFrontExpectingBack flashcards =
    let flashcard = head flashcards
    in
    do
    putStrLn $ Flashcard.presentFront flashcard
    putStrLn $ UIText.backSideQuizPrompt lang
    printPrompt
    userAttempt <- getLine
    putStrLn (if Lesson.checkUserAttemptToProvideBack flashcard userAttempt
              then UIText.correctAnswerMessage lang
              else UIText.incorrectAnswerMessage lang
             )
    quizShowingFrontExpectingBack (tail flashcards) -- ask next question.
