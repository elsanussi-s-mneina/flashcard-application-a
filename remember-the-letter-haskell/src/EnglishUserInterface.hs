module EnglishUserInterface where

welcomeToProgramUIText
  = "Welcome to Remember the Letter (Haskell)"

printingLessonSummaryHeaderUIText
  = "Printing Lesson summary:"

openLessonFileMenuItemShortcutUIText
  = "open"

openLessonFileMenuItemUIText
  = "Enter '" ++ openLessonFileMenuItemShortcutUIText ++ "' if you want to open a lesson file"

createLessonFileMenuItemShortcutUIText
  = "n"
createLessonFileMenuItemUIText
  = "Enter '" ++ createLessonFileMenuItemShortcutUIText ++ "' if you want to create a new lesson.\n"

fileNamePromptAtFileOpeningUIText
  = "Enter a name for a file to open:"

showFrontAndBackMenuItemShortcutUIText
  = "a"

showFrontAndBackMenuItemUIText
  = "Enter '" ++ showFrontAndBackMenuItemShortcutUIText ++ "' to show both front and back of each card."

showFrontMenuItemShortcutUIText
  = "f"

showFrontMenuItemUIText
  = "Enter '" ++ showFrontMenuItemShortcutUIText ++ "' to show the front of each card."

showBackMenuItemShortcutUIText
  = "b"

showBackMenuItemUIText
  = "Enter '" ++ showBackMenuItemShortcutUIText ++ "' to show the back of each card."

addMenuItemShortcutUIText
  = "add"

addMenuItemUIText
  = "Enter '" ++ addMenuItemShortcutUIText ++ "' to add a flashcard."

saveMenuItemShortcutUIText
  = "save"

saveMenuItemUIText
  = "Enter '" ++ saveMenuItemShortcutUIText ++ "' to save all flashcards"

startQuizMenuItemShortcutUIText
  = "start quiz"

startQuizMenuItemUIText
  = "Enter '" ++ startQuizMenuItemShortcutUIText ++ "' to start a quiz"

exitMenuItemShortcutUIText
  = "x"

exitMenuItemUIText
  = "Enter '" ++ exitMenuItemShortcutUIText ++ "' to exit the application."

printFrontsHeaderUIText
  = "Print only fronts of each card:"

printBacksHeaderUIText
  = "Print only backs of each card:"

addingFlashcardHeaderUIText
  = "Adding a flashcard..."

enterFrontSidePromptUIText
  = "Enter the front side:"

enterBackSidePromptUIText
  = "Enter the back side:"

doneAddingFlashcardMessageUIText
  = "Done adding flashcard."

backSideQuizPromptUIText
  = "What is its back side?"

correctAnswerMessageUIText
  = "Correct!"

incorrectAnswerMessageUIText
  = "Incorrect!"

enterFrontSideAcknowledgementUIText
  = "You entered the following for the front side:"

enterBackSideAcknowledgementUIText
  = "You entered the following for the back side:"

multipleCardQuizNotImplementedMessageUIText
  = "Sorry, a quiz with more than one card, is not implemented yet!"

nameOfFileToSavePromptUIText
  = "Enter a name for a file to save to:"
  
savingFlashcardsMessageUIText
  = "Saving flashcards to file called "

doneSavingFileMessageUIText
  = "Done writing to file named" 
  
unrecognizedInputMessageUIText
  = "Unrecognized input:"