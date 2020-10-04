{-|
Module      : DummyUserInterface
Description : This module contains text that is used to test whether internationalization works properly
  without requiring the programmer to know more than one language.
-}
module DummyUserInterface where

import Prelude ((++))
-- This module is not meant to be used by the user, but
-- in the case where we need to make sure the language
-- is structured in a way to easily support multiple languages.

-- This module, gives text for the user interface
-- in English, but with a prefix, so we can easily demonstrate,
-- or test the basic internationalization abilities of the program.

prefix = "LLL " -- We need something to make the text different.
                   -- The text itself is meaningless. It just needs to be short.
                   -- I prefer between 1 and 3 characters.

welcomeToProgram 
  = prefix ++ "Welcome to Remember the Letter (Haskell)"

printingLessonSummaryHeader 
  = prefix ++ "Printing Lesson summary:"

openLessonFileMenuItemShortcut 
  = "open"

openLessonFileMenuItem 
  = prefix ++ "Enter '" ++ openLessonFileMenuItemShortcut ++ "' if you want to open a lesson file"

createLessonFileMenuItemShortcut 
  = prefix ++ "n"

createLessonFileMenuItem 
  = prefix ++ "Enter '" ++ createLessonFileMenuItemShortcut ++ "' if you want to create a new lesson.\n"

fileNamePromptAtFileOpening 
  = prefix ++ "Enter a name for a file to open:"

showFrontAndBackMenuItemShortcut 
  = prefix ++ "a"

showFrontAndBackMenuItem 
  = prefix ++ "Enter '" ++ showFrontAndBackMenuItemShortcut ++ "' to show both front and back of each card."

showFrontMenuItemShortcut 
  = prefix ++ "f"

showFrontMenuItem 
  = prefix ++ "Enter '" ++ showFrontMenuItemShortcut ++ "' to show the front of each card."


showBackMenuItemShortcut 
  = prefix ++ "b"

showBackMenuItem 
  = prefix ++ "Enter '" ++ showBackMenuItemShortcut ++ "' to show the back of each card."

addMenuItemShortcut 
  = prefix ++ "add"

addMenuItem 
  = prefix ++ "Enter '" ++ addMenuItemShortcut ++ "' to add a flashcard."

saveMenuItemShortcut 
  = prefix ++ "save"

saveMenuItem 
  = prefix ++ "Enter '" ++ saveMenuItemShortcut ++ "' to save all flashcards"

startQuizMenuItemShortcut 
  = prefix ++ "start quiz"

startQuizMenuItem 
  = prefix ++ "Enter '" ++ startQuizMenuItem ++ "' to start a quiz"

exitMenuItemShortcut 
  = prefix ++ "x"

exitMenuItem 
  = prefix ++ "Enter '" ++ exitMenuItemShortcut ++ "' to exit the application."

printFrontsHeader 
  = prefix ++ "Print only fronts of each card:"

printBacksHeader 
  = prefix ++ "Print only backs of each card:"

addingFlashcardHeader 
  = prefix ++ "Adding a flashcard..."

enterFrontSidePrompt 
  = prefix ++ "Enter the front side:"

enterBackSidePrompt 
  = prefix ++ "Enter the back side:"

doneAddingFlashcardMessage 
  = prefix ++ "Done adding flashcard."

backSideQuizPrompt 
  = prefix ++ "What is its back side?"

correctAnswerMessage 
  = prefix ++ "Correct!"

incorrectAnswerMessage 
  = prefix ++ "Incorrect!"

enterFrontSideAcknowledgement 
  = prefix ++ "You entered the following for the front side:"

enterBackSideAcknowledgement 
  = prefix ++ "You entered the following for the back side:"

multipleCardQuizNotImplementedMessage 
  = prefix ++ "Sorry, a quiz with more than one card, is not implemented yet!"

nameOfFileToSavePrompt 
  = prefix ++ "Enter a name for a file to save to:"

savingFlashcardsMessage 
  = prefix ++ "Saving flashcards to file called"

doneSavingFileMessage 
  = prefix ++ "Done writing to file named"

unrecognizedInputMessage 
  = prefix ++ "Unrecognized input:"