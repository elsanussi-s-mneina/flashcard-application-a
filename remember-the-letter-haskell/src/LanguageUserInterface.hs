module LanguageUserInterface where

import Prelude ((==), otherwise)
import EnglishUserInterface as En
import DummyUserInterface as Zz



welcomeToProgramUIText langCode
  | langCode == "en" = En.welcomeToProgramUIText
  | otherwise        = Zz.welcomeToProgramUIText

printingLessonSummaryHeaderUIText langCode
  | langCode == "en" = En.printingLessonSummaryHeaderUIText
  | otherwise        = Zz.printingLessonSummaryHeaderUIText

openLessonFileMenuItemShortcutUIText langCode
  | langCode == "en" = En.openLessonFileMenuItemShortcutUIText
  | otherwise        = Zz.openLessonFileMenuItemShortcutUIText

openLessonFileMenuItemUIText langCode
  | langCode == "en" = En.openLessonFileMenuItemUIText
  | otherwise        = Zz.openLessonFileMenuItemUIText

createLessonFileMenuItemShortcutUIText langCode
  | langCode == "en" = En.createLessonFileMenuItemShortcutUIText
  | otherwise        = Zz.createLessonFileMenuItemShortcutUIText

createLessonFileMenuItemUIText langCode
  | langCode == "en" = En.createLessonFileMenuItemUIText
  | otherwise        = Zz.createLessonFileMenuItemUIText

fileNamePromptAtFileOpeningUIText langCode
  | langCode == "en" = En.fileNamePromptAtFileOpeningUIText
  | otherwise        = Zz.fileNamePromptAtFileOpeningUIText

showFrontAndBackMenuItemShortcutUIText langCode
  | langCode == "en" = En.showFrontAndBackMenuItemShortcutUIText
  | otherwise        = Zz.showFrontAndBackMenuItemShortcutUIText

showFrontAndBackMenuItemUIText langCode
  | langCode == "en" = En.showFrontAndBackMenuItemUIText
  | otherwise        = Zz.showFrontAndBackMenuItemUIText

showFrontMenuItemShortcutUIText langCode
  | langCode == "en" = En.showFrontMenuItemShortcutUIText
  | otherwise        = Zz.showFrontMenuItemShortcutUIText

showFrontMenuItemUIText langCode
  | langCode == "en" = En.showFrontMenuItemUIText
  | otherwise        = Zz.showFrontMenuItemUIText

showBackMenuItemShortcutUIText langCode
  | langCode == "en" = En.showBackMenuItemShortcutUIText
  | otherwise        = Zz.showBackMenuItemShortcutUIText

showBackMenuItemUIText langCode
  | langCode == "en" = En.showBackMenuItemUIText
  | otherwise        = Zz.showBackMenuItemUIText

addMenuItemShortcutUIText langCode
  | langCode == "en" = En.addMenuItemShortcutUIText
  | otherwise        = Zz.addMenuItemShortcutUIText

addMenuItemUIText langCode
  | langCode == "en" = En.addMenuItemUIText
  | otherwise        = Zz.addMenuItemUIText


saveMenuItemShortcutUIText langCode
  | langCode == "en" = En.saveMenuItemShortcutUIText
  | otherwise        = Zz.saveMenuItemShortcutUIText

saveMenuItemUIText langCode
  | langCode == "en" = En.saveMenuItemUIText
  | otherwise        = Zz.saveMenuItemUIText

startQuizMenuItemShortcutUIText langCode
  | langCode == "en" = En.startQuizMenuItemShortcutUIText
  | otherwise        = Zz.startQuizMenuItemShortcutUIText

startQuizMenuItemUIText langCode
  | langCode == "en" = En.startQuizMenuItemUIText
  | otherwise        = Zz.startQuizMenuItemUIText

exitMenuItemShortcutUIText langCode
  | langCode == "en" = En.exitMenuItemShortcutUIText
  | otherwise        = Zz.exitMenuItemShortcutUIText

exitMenuItemUIText langCode
  | langCode == "en" = En.exitMenuItemUIText
  | otherwise        = Zz.exitMenuItemUIText

printFrontsHeaderUIText langCode
  | langCode == "en" = En.printFrontsHeaderUIText
  | otherwise        = Zz.printFrontsHeaderUIText

printBacksHeaderUIText langCode
  | langCode == "en" = En.printBacksHeaderUIText
  | otherwise        = Zz.printBacksHeaderUIText

addingFlashcardHeaderUIText langCode
  | langCode == "en" = En.addingFlashcardHeaderUIText
  | otherwise        = Zz.addingFlashcardHeaderUIText

enterFrontSidePromptUIText langCode
  | langCode == "en" = En.enterFrontSidePromptUIText
  | otherwise        = Zz.enterFrontSidePromptUIText

enterBackSidePromptUIText langCode
  | langCode == "en" = En.enterBackSidePromptUIText
  | otherwise        = Zz.enterBackSidePromptUIText

doneAddingFlashcardMessageUIText langCode
  | langCode == "en" = En.doneAddingFlashcardMessageUIText
  | otherwise        = Zz.doneAddingFlashcardMessageUIText

backSideQuizPromptUIText langCode
  | langCode == "en" = En.backSideQuizPromptUIText
  | otherwise        = Zz.backSideQuizPromptUIText

correctAnswerMessageUIText langCode
  | langCode == "en" = En.correctAnswerMessageUIText
  | otherwise        = Zz.correctAnswerMessageUIText

incorrectAnswerMessageUIText langCode
  | langCode == "en" = En.incorrectAnswerMessageUIText
  | otherwise        = Zz.incorrectAnswerMessageUIText

enterFrontSideAcknowledgementUIText langCode
  | langCode == "en" = En.enterFrontSideAcknowledgementUIText
  | otherwise        = Zz.enterFrontSideAcknowledgementUIText

enterBackSideAcknowledgementUIText langCode
  | langCode == "en" = En.enterBackSideAcknowledgementUIText
  | otherwise        = Zz.enterBackSideAcknowledgementUIText

multipleCardQuizNotImplementedMessageUIText langCode
  | langCode == "en" = En.multipleCardQuizNotImplementedMessageUIText
  | otherwise        = Zz.multipleCardQuizNotImplementedMessageUIText

nameOfFileToSavePromptUIText langCode
  | langCode == "en" = En.nameOfFileToSavePromptUIText
  | otherwise        = Zz.nameOfFileToSavePromptUIText

savingFlashcardsMessageUIText langCode
  | langCode == "en" = En.savingFlashcardsMessageUIText
  | otherwise        = Zz.savingFlashcardsMessageUIText

doneSavingFileMessageUIText langCode
  | langCode == "en" = En.doneSavingFileMessageUIText
  | otherwise        = Zz.doneSavingFileMessageUIText

unrecognizedInputMessageUIText langCode
  | langCode == "en" = En.unrecognizedInputMessageUIText
  | otherwise        = Zz.unrecognizedInputMessageUIText

{-- Template:
UIText langCode
  | langCode == "en" = En.
  | otherwise        = Zz.
--}
