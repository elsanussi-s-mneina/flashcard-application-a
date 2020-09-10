{-|
Module      : DummyUserInterface
Description : This module contains text that is used to test whether internationalization works properly
  without requiring the programmer to know more than one language.
-}
module DummyUserInterface where

-- This module is not meant to be used by the user, but
-- in the case where we need to make sure the language
-- is structured in a way to easily support multiple languages.

-- This module, gives text for the user interface
-- in English, but with a prefix, so we can easily demonstrate,
-- or test the basic internationalization abilities of the program.

prefix = "LLL " -- We need something to make the text different.
                   -- The text itself is meaningless. It just needs to be short.
                   -- I prefer between 1 and 3 characters.

welcomeToProgramUIText
  = prefix ++ "Welcome to Remember the Letter (Haskell)"

printingLessonSummaryHeaderUIText
  = prefix ++ "Printing Lesson summary:"

openLessonFileMenuItemShortcutUIText
  = "open"

openLessonFileMenuItemUIText
  = prefix ++ "Enter '" ++ openLessonFileMenuItemShortcutUIText ++ "' if you want to open a lesson file"

createLessonFileMenuItemShortcutUIText
  = prefix ++ "n"

createLessonFileMenuItemUIText
  = prefix ++ "Enter '" ++ createLessonFileMenuItemShortcutUIText ++ "' if you want to create a new lesson.\n"

fileNamePromptAtFileOpeningUIText
  = prefix ++ "Enter a name for a file to open:"

showFrontAndBackMenuItemShortcutUIText
  = prefix ++ "a"

showFrontAndBackMenuItemUIText
  = prefix ++ "Enter '" ++ showFrontAndBackMenuItemShortcutUIText ++ "' to show both front and back of each card."

showFrontMenuItemShortcutUIText
  = prefix ++ "f"

showFrontMenuItemUIText
  = prefix ++ "Enter '" ++ showFrontMenuItemShortcutUIText ++ "' to show the front of each card."


showBackMenuItemShortcutUIText
  = prefix ++ "b"

showBackMenuItemUIText
  = prefix ++ "Enter '" ++ showBackMenuItemShortcutUIText ++ "' to show the back of each card."

addMenuItemShortcutUIText
  = prefix ++ "add"

addMenuItemUIText
  = prefix ++ "Enter '" ++ addMenuItemShortcutUIText ++ "' to add a flashcard."

saveMenuItemShortcutUIText
  = prefix ++ "save"

saveMenuItemUIText
  = prefix ++ "Enter '" ++ saveMenuItemShortcutUIText ++ "' to save all flashcards"

startQuizMenuItemShortcutUIText
  = prefix ++ "start quiz"

startQuizMenuItemUIText
  = prefix ++ "Enter '" ++ startQuizMenuItemUIText ++ "' to start a quiz"

exitMenuItemShortcutUIText
  = prefix ++ "x"

exitMenuItemUIText
  = prefix ++ "Enter '" ++ exitMenuItemShortcutUIText ++ "' to exit the application."

printFrontsHeaderUIText
  = prefix ++ "Print only fronts of each card:"

printBacksHeaderUIText
  = prefix ++ "Print only backs of each card:"

addingFlashcardHeaderUIText
  = prefix ++ "Adding a flashcard..."

enterFrontSidePromptUIText
  = prefix ++ "Enter the front side:"

enterBackSidePromptUIText
  = prefix ++ "Enter the back side:"

doneAddingFlashcardMessageUIText
  = prefix ++ "Done adding flashcard."

backSideQuizPromptUIText
  = prefix ++ "What is its back side?"

correctAnswerMessageUIText
  = prefix ++ "Correct!"

incorrectAnswerMessageUIText
  = prefix ++ "Incorrect!"

enterFrontSideAcknowledgementUIText
  = prefix ++ "You entered the following for the front side:"

enterBackSideAcknowledgementUIText
  = prefix ++ "You entered the following for the back side:"

multipleCardQuizNotImplementedMessageUIText
  = prefix ++ "Sorry, a quiz with more than one card, is not implemented yet!"

nameOfFileToSavePromptUIText
  = prefix ++ "Enter a name for a file to save to:"

savingFlashcardsMessageUIText
  = prefix ++ "Saving flashcards to file called"

doneSavingFileMessageUIText
  = prefix ++ "Done writing to file named"

unrecognizedInputMessageUIText
  = prefix ++ "Unrecognized input:"