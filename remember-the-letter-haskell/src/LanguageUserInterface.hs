module LanguageUserInterface where

import Prelude ((==), String, otherwise)
import EnglishUserInterface as En
import DummyUserInterface as Zz
import LanguageIDs

welcomeToProgramUIText :: TwoLetterLanguageID -> String
welcomeToProgramUIText langCode
  | langCode == EN = En.welcomeToProgramUIText
  | otherwise      = Zz.welcomeToProgramUIText

printingLessonSummaryHeaderUIText :: TwoLetterLanguageID -> String
printingLessonSummaryHeaderUIText langCode
  | langCode == EN = En.printingLessonSummaryHeaderUIText
  | otherwise      = Zz.printingLessonSummaryHeaderUIText

openLessonFileMenuItemShortcutUIText :: TwoLetterLanguageID -> String
openLessonFileMenuItemShortcutUIText langCode
  | langCode == EN = En.openLessonFileMenuItemShortcutUIText
  | otherwise      = Zz.openLessonFileMenuItemShortcutUIText

openLessonFileMenuItemUIText :: TwoLetterLanguageID -> String
openLessonFileMenuItemUIText langCode
  | langCode == EN = En.openLessonFileMenuItemUIText
  | otherwise      = Zz.openLessonFileMenuItemUIText

createLessonFileMenuItemShortcutUIText :: TwoLetterLanguageID -> String
createLessonFileMenuItemShortcutUIText langCode
  | langCode == EN = En.createLessonFileMenuItemShortcutUIText
  | otherwise      = Zz.createLessonFileMenuItemShortcutUIText

createLessonFileMenuItemUIText :: TwoLetterLanguageID -> String
createLessonFileMenuItemUIText langCode
  | langCode == EN = En.createLessonFileMenuItemUIText
  | otherwise      = Zz.createLessonFileMenuItemUIText
 
fileNamePromptAtFileOpeningUIText :: TwoLetterLanguageID -> String
fileNamePromptAtFileOpeningUIText langCode
  | langCode == EN = En.fileNamePromptAtFileOpeningUIText
  | otherwise      = Zz.fileNamePromptAtFileOpeningUIText

showFrontAndBackMenuItemShortcutUIText :: TwoLetterLanguageID -> String
showFrontAndBackMenuItemShortcutUIText langCode
  | langCode == EN = En.showFrontAndBackMenuItemShortcutUIText
  | otherwise        = Zz.showFrontAndBackMenuItemShortcutUIText

showFrontAndBackMenuItemUIText :: TwoLetterLanguageID -> String
showFrontAndBackMenuItemUIText langCode
  | langCode == EN = En.showFrontAndBackMenuItemUIText
  | otherwise        = Zz.showFrontAndBackMenuItemUIText

showFrontMenuItemShortcutUIText :: TwoLetterLanguageID -> String
showFrontMenuItemShortcutUIText langCode
  | langCode == EN = En.showFrontMenuItemShortcutUIText
  | otherwise        = Zz.showFrontMenuItemShortcutUIText

showFrontMenuItemUIText :: TwoLetterLanguageID -> String
showFrontMenuItemUIText langCode
  | langCode == EN = En.showFrontMenuItemUIText
  | otherwise      = Zz.showFrontMenuItemUIText

showBackMenuItemShortcutUIText :: TwoLetterLanguageID -> String
showBackMenuItemShortcutUIText langCode
  | langCode == EN = En.showBackMenuItemShortcutUIText
  | otherwise      = Zz.showBackMenuItemShortcutUIText

showBackMenuItemUIText :: TwoLetterLanguageID -> String
showBackMenuItemUIText langCode
  | langCode == EN = En.showBackMenuItemUIText
  | otherwise      = Zz.showBackMenuItemUIText

addMenuItemShortcutUIText :: TwoLetterLanguageID -> String
addMenuItemShortcutUIText langCode
  | langCode == EN = En.addMenuItemShortcutUIText
  | otherwise      = Zz.addMenuItemShortcutUIText

addMenuItemUIText :: TwoLetterLanguageID -> String
addMenuItemUIText langCode
  | langCode == EN = En.addMenuItemUIText
  | otherwise      = Zz.addMenuItemUIText

saveMenuItemShortcutUIText :: TwoLetterLanguageID -> String
saveMenuItemShortcutUIText langCode
  | langCode == EN = En.saveMenuItemShortcutUIText
  | otherwise      = Zz.saveMenuItemShortcutUIText

saveMenuItemUIText :: TwoLetterLanguageID -> String
saveMenuItemUIText langCode
  | langCode == EN = En.saveMenuItemUIText
  | otherwise      = Zz.saveMenuItemUIText

startQuizMenuItemShortcutUIText :: TwoLetterLanguageID -> String
startQuizMenuItemShortcutUIText langCode
  | langCode == EN = En.startQuizMenuItemShortcutUIText
  | otherwise      = Zz.startQuizMenuItemShortcutUIText

startQuizMenuItemUIText :: TwoLetterLanguageID -> String
startQuizMenuItemUIText langCode
  | langCode == EN = En.startQuizMenuItemUIText
  | otherwise      = Zz.startQuizMenuItemUIText

exitMenuItemShortcutUIText :: TwoLetterLanguageID -> String
exitMenuItemShortcutUIText langCode
  | langCode == EN = En.exitMenuItemShortcutUIText
  | otherwise      = Zz.exitMenuItemShortcutUIText

exitMenuItemUIText :: TwoLetterLanguageID -> String
exitMenuItemUIText langCode
  | langCode == EN = En.exitMenuItemUIText
  | otherwise      = Zz.exitMenuItemUIText

printFrontsHeaderUIText :: TwoLetterLanguageID -> String
printFrontsHeaderUIText langCode
  | langCode == EN = En.printFrontsHeaderUIText
  | otherwise      = Zz.printFrontsHeaderUIText

printBacksHeaderUIText :: TwoLetterLanguageID -> String
printBacksHeaderUIText langCode
  | langCode == EN = En.printBacksHeaderUIText
  | otherwise      = Zz.printBacksHeaderUIText

addingFlashcardHeaderUIText :: TwoLetterLanguageID -> String
addingFlashcardHeaderUIText langCode
  | langCode == EN = En.addingFlashcardHeaderUIText
  | otherwise      = Zz.addingFlashcardHeaderUIText

enterFrontSidePromptUIText :: TwoLetterLanguageID -> String
enterFrontSidePromptUIText langCode
  | langCode == EN = En.enterFrontSidePromptUIText
  | otherwise      = Zz.enterFrontSidePromptUIText

enterBackSidePromptUIText :: TwoLetterLanguageID -> String
enterBackSidePromptUIText langCode
  | langCode == EN = En.enterBackSidePromptUIText
  | otherwise      = Zz.enterBackSidePromptUIText

doneAddingFlashcardMessageUIText :: TwoLetterLanguageID -> String
doneAddingFlashcardMessageUIText langCode
  | langCode == EN = En.doneAddingFlashcardMessageUIText
  | otherwise      = Zz.doneAddingFlashcardMessageUIText

backSideQuizPromptUIText :: TwoLetterLanguageID -> String
backSideQuizPromptUIText langCode
  | langCode == EN = En.backSideQuizPromptUIText
  | otherwise      = Zz.backSideQuizPromptUIText

correctAnswerMessageUIText :: TwoLetterLanguageID -> String
correctAnswerMessageUIText langCode
  | langCode == EN = En.correctAnswerMessageUIText
  | otherwise      = Zz.correctAnswerMessageUIText

incorrectAnswerMessageUIText :: TwoLetterLanguageID -> String
incorrectAnswerMessageUIText langCode
  | langCode == EN = En.incorrectAnswerMessageUIText
  | otherwise      = Zz.incorrectAnswerMessageUIText

enterFrontSideAcknowledgementUIText :: TwoLetterLanguageID -> String
enterFrontSideAcknowledgementUIText langCode
  | langCode == EN = En.enterFrontSideAcknowledgementUIText
  | otherwise      = Zz.enterFrontSideAcknowledgementUIText

enterBackSideAcknowledgementUIText :: TwoLetterLanguageID -> String
enterBackSideAcknowledgementUIText langCode
  | langCode == EN = En.enterBackSideAcknowledgementUIText
  | otherwise      = Zz.enterBackSideAcknowledgementUIText

multipleCardQuizNotImplementedMessageUIText :: TwoLetterLanguageID -> String
multipleCardQuizNotImplementedMessageUIText langCode
  | langCode == EN = En.multipleCardQuizNotImplementedMessageUIText
  | otherwise      = Zz.multipleCardQuizNotImplementedMessageUIText

nameOfFileToSavePromptUIText :: TwoLetterLanguageID -> String
nameOfFileToSavePromptUIText langCode
  | langCode == EN = En.nameOfFileToSavePromptUIText
  | otherwise      = Zz.nameOfFileToSavePromptUIText

savingFlashcardsMessageUIText :: TwoLetterLanguageID -> String
savingFlashcardsMessageUIText langCode
  | langCode == EN = En.savingFlashcardsMessageUIText
  | otherwise      = Zz.savingFlashcardsMessageUIText

doneSavingFileMessageUIText :: TwoLetterLanguageID -> String
doneSavingFileMessageUIText langCode
  | langCode == EN = En.doneSavingFileMessageUIText
  | otherwise      = Zz.doneSavingFileMessageUIText

unrecognizedInputMessageUIText :: TwoLetterLanguageID -> String
unrecognizedInputMessageUIText langCode
  | langCode == EN = En.unrecognizedInputMessageUIText
  | otherwise      = Zz.unrecognizedInputMessageUIText

{-- Template:
:: TwoLetterLanguageID -> String
UIText langCode
  | langCode == EN = En.
  | otherwise      = Zz.
--}
