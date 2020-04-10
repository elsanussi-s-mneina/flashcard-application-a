{-|
Module      : LearnerLesson
Description : User data with respect to a lesson. A lesson consists of flashcard data, and related functions.
-}
module LearnerLesson where

import Lesson(Flashcard(Flashcard), back, front)

-- A flashcard with user specific information.
data LearnerFlashcard = LearnerFlashcard
  { flashcard :: Flashcard
  , correctCount :: Int
  , attemptCount :: Int
  } 
  deriving (Show, Eq)

learnerAnswersCorrectly :: LearnerFlashcard -> LearnerFlashcard
learnerAnswersCorrectly (LearnerFlashcard {flashcard = f, correctCount = cc, attemptCount = ac}) = 
  LearnerFlashcard {flashcard =  f, correctCount = 1 + cc, attemptCount = 1 + ac}

learnerAnswersIncorrectly :: LearnerFlashcard -> LearnerFlashcard
learnerAnswersIncorrectly (LearnerFlashcard {flashcard = f, correctCount = cc, attemptCount = ac}) = 
  LearnerFlashcard {flashcard =  f, correctCount = cc, attemptCount = ac + 1}
