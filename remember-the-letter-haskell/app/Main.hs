{-|
Module      : Main
Description : This module starts the program. The program runs on the terminal. The program shows text,
and waits for user input.

All input and output happen in this module.
-}
module Main (main) where

import Prelude (IO, ($), (++), (==), getLine, head, putStr, putStrLn, return, tail, null)

import qualified Lesson (addFlashcard, backSummary, Flashcard, frontSummary,
               summary, toTabSeparatedValues,
               fromTabSeparatedValues, checkUserAttemptToProvideBack, presentFrontOfFlashcard)
import System.IO (hFlush, readFile, stdout, writeFile)
import System.Exit (exitSuccess)
import Control.Monad (unless)
import LanguageUserInterface
  ( welcomeToProgramUIText
  , printingLessonSummaryHeaderUIText
  , openLessonFileMenuItemShortcutUIText
  , openLessonFileMenuItemUIText
  , createLessonFileMenuItemShortcutUIText
  , createLessonFileMenuItemUIText
  , fileNamePromptAtFileOpeningUIText
  , showFrontAndBackMenuItemUIText
  , showFrontAndBackMenuItemShortcutUIText
  , showFrontMenuItemUIText
  , showFrontMenuItemShortcutUIText
  , showBackMenuItemUIText
  , showBackMenuItemShortcutUIText
  , addMenuItemUIText
  , addMenuItemShortcutUIText
  , saveMenuItemUIText
  , saveMenuItemShortcutUIText
  , startQuizMenuItemUIText
  , startQuizMenuItemShortcutUIText
  , exitMenuItemShortcutUIText
  , exitMenuItemUIText
  , printFrontsHeaderUIText
  , printBacksHeaderUIText
  , addingFlashcardHeaderUIText
  , enterFrontSidePromptUIText
  , enterBackSidePromptUIText
  , doneAddingFlashcardMessageUIText
  , backSideQuizPromptUIText
  , correctAnswerMessageUIText
  , incorrectAnswerMessageUIText
  , enterFrontSideAcknowledgementUIText
  , enterBackSideAcknowledgementUIText
  , nameOfFileToSavePromptUIText
  , savingFlashcardsMessageUIText
  , doneSavingFileMessageUIText
  , unrecognizedInputMessageUIText
  )

import LanguageIDs (TwoLetterLanguageID(EN))

lang :: TwoLetterLanguageID
lang = EN

-- | This program runs in the terminal. It outputs
-- text to the student.
main :: IO ()
main =
  do
  putStrLn $ welcomeToProgramUIText lang
  putStrLn ""
  putStrLn $ openLessonFileMenuItemUIText lang
  putStrLn $ createLessonFileMenuItemUIText lang
  putStrLn $ exitMenuItemUIText lang
  printPrompt

  userInput <- getLine
  case userInput of
        _ | userInput == openLessonFileMenuItemShortcutUIText lang
                 ->
                 do
                 putStrLn $ fileNamePromptAtFileOpeningUIText lang
                 printPrompt
                 fileName <- getLine
                 fileContents <- readFile fileName
                 let flashcards' = Lesson.fromTabSeparatedValues fileContents
                 commandLineLoop flashcards'
                 return ()
        _ | userInput == createLessonFileMenuItemShortcutUIText lang
                 ->
                 commandLineLoop []
        _ | userInput == exitMenuItemShortcutUIText lang ->  exitSuccess
        _
                 ->
                 do
                 putStrLn $ unrecognizedInputMessageUIText lang ++ " \"" ++ userInput ++  "\""
                 main

commandLineLoop :: [Lesson.Flashcard] -> IO ()
commandLineLoop flashcards =
  do
  putStrLn $ showFrontAndBackMenuItemUIText lang
  putStrLn $ showFrontMenuItemUIText lang
  putStrLn $ showBackMenuItemUIText lang
  putStrLn $ addMenuItemUIText lang
  putStrLn $ saveMenuItemUIText lang

  unless (null flashcards) (putStrLn $ startQuizMenuItemUIText lang)

  putStrLn $ exitMenuItemUIText lang
  putStrLn "" -- blank line
  printPrompt
  userInput <- getLine

  case userInput of
       _ | userInput == showFrontAndBackMenuItemShortcutUIText lang
              ->
              do
              putStrLn $ printingLessonSummaryHeaderUIText lang
              putStrLn (Lesson.summary flashcards)
       _ | userInput == showFrontMenuItemShortcutUIText lang
              ->
              do
              putStrLn $ printFrontsHeaderUIText lang
              putStrLn (Lesson.frontSummary flashcards)
       _ | userInput == showBackMenuItemShortcutUIText lang
              ->
              do
              putStrLn $ printBacksHeaderUIText lang
              putStrLn (Lesson.backSummary flashcards)
       _ | userInput == addMenuItemShortcutUIText lang
              ->
              do
              putStrLn $ addingFlashcardHeaderUIText lang
              putStrLn $ enterFrontSidePromptUIText lang
              printPrompt
              fSide <- getLine
              putStrLn (enterFrontSideAcknowledgementUIText lang ++ " (" ++
                         fSide ++ ")")
              putStrLn $ enterBackSidePromptUIText lang
              printPrompt
              bSide  <- getLine
              putStrLn (enterBackSideAcknowledgementUIText lang ++ " (" ++
                         bSide ++ ")")
              let flashcards' = Lesson.addFlashcard flashcards fSide bSide
              putStrLn $ doneAddingFlashcardMessageUIText lang
              commandLineLoop flashcards' -- loop (go back to the beginning)
       _ | userInput == startQuizMenuItemShortcutUIText lang
              ->
              do
              quizShowingFrontExpectingBack flashcards
       _ | userInput == saveMenuItemShortcutUIText lang
              ->
              do
              -- Let the user choose the file name.
              putStrLn $ nameOfFileToSavePromptUIText lang
              printPrompt
              fileName <- getLine

              putStrLn (savingFlashcardsMessageUIText lang ++  "'" ++ fileName ++ "'")
              _ <- writeFile fileName (Lesson.toTabSeparatedValues flashcards)
              putStrLn (doneSavingFileMessageUIText lang ++ " " ++ fileName)
       _ | userInput == exitMenuItemShortcutUIText lang ->  exitSuccess
       _      ->  putStrLn (unrecognizedInputMessageUIText lang ++ " (" ++ userInput ++ ")")
  commandLineLoop flashcards -- loop (go back to the beginning)

printPrompt :: IO ()
printPrompt =
  do
  putStr  "> "  -- terminal prompt to show the user
  hFlush stdout -- We need to flush standard out
                -- so that the terminal prompt appears
                -- before the user input rather than after.

-- | Asks user to remember the back side of a flashcard .
quizShowingFrontExpectingBack :: [Lesson.Flashcard] -> IO ()
quizShowingFrontExpectingBack [] = return ()
quizShowingFrontExpectingBack flashcards =
    let flashcard = head flashcards
    in
    do
    putStrLn $ Lesson.presentFrontOfFlashcard flashcard
    putStrLn $ backSideQuizPromptUIText lang
    printPrompt
    userAttempt <- getLine
    putStrLn (if Lesson.checkUserAttemptToProvideBack flashcard userAttempt
              then correctAnswerMessageUIText lang
              else incorrectAnswerMessageUIText lang
             )
    quizShowingFrontExpectingBack (tail flashcards) -- ask next question.
