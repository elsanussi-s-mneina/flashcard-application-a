{-|
Module      : LanguageUserInterface
Description : allows the program's user interface to support more than one language
-}
module LanguageUserInterface where

import Prelude ((==), String, otherwise)
import EnglishUserInterface as En
import DummyUserInterface as Zz
import LanguageIDs

welcomeToProgram :: TwoLetterLanguageID -> String
welcomeToProgram langCode
  | langCode == EN = En.welcomeToProgram 
  | otherwise      = Zz.welcomeToProgram 

printingLessonSummaryHeader :: TwoLetterLanguageID -> String
printingLessonSummaryHeader langCode
  | langCode == EN = En.printingLessonSummaryHeader 
  | otherwise      = Zz.printingLessonSummaryHeader 

openLessonFileMenuItemShortcut :: TwoLetterLanguageID -> String
openLessonFileMenuItemShortcut langCode
  | langCode == EN = En.openLessonFileMenuItemShortcut 
  | otherwise      = Zz.openLessonFileMenuItemShortcut 

openLessonFileMenuItem :: TwoLetterLanguageID -> String
openLessonFileMenuItem langCode
  | langCode == EN = En.openLessonFileMenuItem 
  | otherwise      = Zz.openLessonFileMenuItem 

createLessonFileMenuItemShortcut :: TwoLetterLanguageID -> String
createLessonFileMenuItemShortcut langCode
  | langCode == EN = En.createLessonFileMenuItemShortcut 
  | otherwise      = Zz.createLessonFileMenuItemShortcut 

createLessonFileMenuItem :: TwoLetterLanguageID -> String
createLessonFileMenuItem langCode
  | langCode == EN = En.createLessonFileMenuItem 
  | otherwise      = Zz.createLessonFileMenuItem 
 
fileNamePromptAtFileOpening :: TwoLetterLanguageID -> String
fileNamePromptAtFileOpening langCode
  | langCode == EN = En.fileNamePromptAtFileOpening 
  | otherwise      = Zz.fileNamePromptAtFileOpening 

showFrontAndBackMenuItemShortcut :: TwoLetterLanguageID -> String
showFrontAndBackMenuItemShortcut langCode
  | langCode == EN = En.showFrontAndBackMenuItemShortcut 
  | otherwise        = Zz.showFrontAndBackMenuItemShortcut 

showFrontAndBackMenuItem :: TwoLetterLanguageID -> String
showFrontAndBackMenuItem langCode
  | langCode == EN = En.showFrontAndBackMenuItem 
  | otherwise        = Zz.showFrontAndBackMenuItem 

showFrontMenuItemShortcut :: TwoLetterLanguageID -> String
showFrontMenuItemShortcut langCode
  | langCode == EN = En.showFrontMenuItemShortcut 
  | otherwise        = Zz.showFrontMenuItemShortcut 

showFrontMenuItem :: TwoLetterLanguageID -> String
showFrontMenuItem langCode
  | langCode == EN = En.showFrontMenuItem 
  | otherwise      = Zz.showFrontMenuItem 

showBackMenuItemShortcut :: TwoLetterLanguageID -> String
showBackMenuItemShortcut langCode
  | langCode == EN = En.showBackMenuItemShortcut 
  | otherwise      = Zz.showBackMenuItemShortcut 

showBackMenuItem :: TwoLetterLanguageID -> String
showBackMenuItem langCode
  | langCode == EN = En.showBackMenuItem 
  | otherwise      = Zz.showBackMenuItem 

addMenuItemShortcut :: TwoLetterLanguageID -> String
addMenuItemShortcut langCode
  | langCode == EN = En.addMenuItemShortcut 
  | otherwise      = Zz.addMenuItemShortcut 

addMenuItem :: TwoLetterLanguageID -> String
addMenuItem langCode
  | langCode == EN = En.addMenuItem 
  | otherwise      = Zz.addMenuItem 

saveMenuItemShortcut :: TwoLetterLanguageID -> String
saveMenuItemShortcut langCode
  | langCode == EN = En.saveMenuItemShortcut 
  | otherwise      = Zz.saveMenuItemShortcut 

saveMenuItem :: TwoLetterLanguageID -> String
saveMenuItem langCode
  | langCode == EN = En.saveMenuItem 
  | otherwise      = Zz.saveMenuItem 

startQuizMenuItemShortcut :: TwoLetterLanguageID -> String
startQuizMenuItemShortcut langCode
  | langCode == EN = En.startQuizMenuItemShortcut 
  | otherwise      = Zz.startQuizMenuItemShortcut 

startQuizMenuItem :: TwoLetterLanguageID -> String
startQuizMenuItem langCode
  | langCode == EN = En.startQuizMenuItem 
  | otherwise      = Zz.startQuizMenuItem 

exitMenuItemShortcut :: TwoLetterLanguageID -> String
exitMenuItemShortcut langCode
  | langCode == EN = En.exitMenuItemShortcut 
  | otherwise      = Zz.exitMenuItemShortcut 

exitMenuItem :: TwoLetterLanguageID -> String
exitMenuItem langCode
  | langCode == EN = En.exitMenuItem 
  | otherwise      = Zz.exitMenuItem 

printFrontsHeader :: TwoLetterLanguageID -> String
printFrontsHeader langCode
  | langCode == EN = En.printFrontsHeader 
  | otherwise      = Zz.printFrontsHeader 

printBacksHeader :: TwoLetterLanguageID -> String
printBacksHeader langCode
  | langCode == EN = En.printBacksHeader 
  | otherwise      = Zz.printBacksHeader 

addingFlashcardHeader :: TwoLetterLanguageID -> String
addingFlashcardHeader langCode
  | langCode == EN = En.addingFlashcardHeader 
  | otherwise      = Zz.addingFlashcardHeader 

enterFrontSidePrompt :: TwoLetterLanguageID -> String
enterFrontSidePrompt langCode
  | langCode == EN = En.enterFrontSidePrompt 
  | otherwise      = Zz.enterFrontSidePrompt 

enterBackSidePrompt :: TwoLetterLanguageID -> String
enterBackSidePrompt langCode
  | langCode == EN = En.enterBackSidePrompt 
  | otherwise      = Zz.enterBackSidePrompt 

doneAddingFlashcardMessage :: TwoLetterLanguageID -> String
doneAddingFlashcardMessage langCode
  | langCode == EN = En.doneAddingFlashcardMessage 
  | otherwise      = Zz.doneAddingFlashcardMessage 

backSideQuizPrompt :: TwoLetterLanguageID -> String
backSideQuizPrompt langCode
  | langCode == EN = En.backSideQuizPrompt 
  | otherwise      = Zz.backSideQuizPrompt 

correctAnswerMessage :: TwoLetterLanguageID -> String
correctAnswerMessage langCode
  | langCode == EN = En.correctAnswerMessage 
  | otherwise      = Zz.correctAnswerMessage 

incorrectAnswerMessage :: TwoLetterLanguageID -> String
incorrectAnswerMessage langCode
  | langCode == EN = En.incorrectAnswerMessage 
  | otherwise      = Zz.incorrectAnswerMessage 

enterFrontSideAcknowledgement :: TwoLetterLanguageID -> String
enterFrontSideAcknowledgement langCode
  | langCode == EN = En.enterFrontSideAcknowledgement 
  | otherwise      = Zz.enterFrontSideAcknowledgement 

enterBackSideAcknowledgement :: TwoLetterLanguageID -> String
enterBackSideAcknowledgement langCode
  | langCode == EN = En.enterBackSideAcknowledgement 
  | otherwise      = Zz.enterBackSideAcknowledgement 

nameOfFileToSavePrompt :: TwoLetterLanguageID -> String
nameOfFileToSavePrompt langCode
  | langCode == EN = En.nameOfFileToSavePrompt 
  | otherwise      = Zz.nameOfFileToSavePrompt 

savingFlashcardsMessage :: TwoLetterLanguageID -> String
savingFlashcardsMessage langCode
  | langCode == EN = En.savingFlashcardsMessage 
  | otherwise      = Zz.savingFlashcardsMessage 

doneSavingFileMessage :: TwoLetterLanguageID -> String
doneSavingFileMessage langCode
  | langCode == EN = En.doneSavingFileMessage 
  | otherwise      = Zz.doneSavingFileMessage 

unrecognizedInputMessage :: TwoLetterLanguageID -> String
unrecognizedInputMessage langCode
  | langCode == EN = En.unrecognizedInputMessage 
  | otherwise      = Zz.unrecognizedInputMessage 

{-- The following commented out code exists here
as a snippet, so that more functions can be easily
defined by using the template.

Template:
:: TwoLetterLanguageID -> String
 langCode
  | langCode == EN = En.
  | otherwise      = Zz.
--}
