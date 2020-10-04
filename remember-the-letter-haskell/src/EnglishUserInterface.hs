{-|
Module      : EnglishUserInterface
Description : All English text that is presented to the user, which the user did not supply
              goes here.
-}
module EnglishUserInterface where

import Prelude ((++))

welcomeToProgram
  = "Welcome to Remember the Letter (Haskell)"

printingLessonSummaryHeader
  = "Printing Lesson summary:"

openLessonFileMenuItemShortcut
  = "open"

openLessonFileMenuItem
  = "Enter '" ++ openLessonFileMenuItemShortcut ++ "' if you want to open a lesson file"

createLessonFileMenuItemShortcut
  = "n"
createLessonFileMenuItem
  = "Enter '" ++ createLessonFileMenuItemShortcut ++ "' if you want to create a new lesson.\n"

fileNamePromptAtFileOpening
  = "Enter a name for a file to open:"

showFrontAndBackMenuItemShortcut
  = "a"

showFrontAndBackMenuItem
  = "Enter '" ++ showFrontAndBackMenuItemShortcut ++ "' to show both front and back of each card."

showFrontMenuItemShortcut
  = "f"

showFrontMenuItem
  = "Enter '" ++ showFrontMenuItemShortcut ++ "' to show the front of each card."

showBackMenuItemShortcut
  = "b"

showBackMenuItem
  = "Enter '" ++ showBackMenuItemShortcut ++ "' to show the back of each card."

addMenuItemShortcut
  = "add"

addMenuItem
  = "Enter '" ++ addMenuItemShortcut ++ "' to add a flashcard."

saveMenuItemShortcut
  = "save"

saveMenuItem
  = "Enter '" ++ saveMenuItemShortcut ++ "' to save all flashcards"

startQuizMenuItemShortcut
  = "start quiz"

startQuizMenuItem
  = "Enter '" ++ startQuizMenuItemShortcut ++ "' to start a quiz"

exitMenuItemShortcut
  = "x"

exitMenuItem
  = "Enter '" ++ exitMenuItemShortcut ++ "' to exit the application."

printFrontsHeader
  = "Print only fronts of each card:"

printBacksHeader
  = "Print only backs of each card:"

addingFlashcardHeader
  = "Adding a flashcard..."

enterFrontSidePrompt
  = "Enter the front side:"

enterBackSidePrompt
  = "Enter the back side:"

doneAddingFlashcardMessage
  = "Done adding flashcard."

backSideQuizPrompt
  = "What is its back side?"

correctAnswerMessage
  = "Correct!"

incorrectAnswerMessage
  = "Incorrect!"

enterFrontSideAcknowledgement
  = "You entered the following for the front side:"

enterBackSideAcknowledgement
  = "You entered the following for the back side:"

nameOfFileToSavePrompt
  = "Enter a name for a file to save to:"
  
savingFlashcardsMessage
  = "Saving flashcards to file called "

doneSavingFileMessage
  = "Done writing to file named" 
  
unrecognizedInputMessage
  = "Unrecognized input:"