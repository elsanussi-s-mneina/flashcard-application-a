{-|
Module      : LearnerLesson
Description : User data with respect to a lesson. A lesson consists of flashcard data, and related functions.
-}
module LearnerLesson where

import Lesson(Flashcard(Flashcard), back, front)

-- A flashcard with user specific information.
data LearnerFlashcard = LearnerFlashcard
  { flashcard :: Flashcard
  , score :: Int
  } 
  deriving (Show, Eq)

learnerAnswersCorrectly :: LearnerFlashcard -> LearnerFlashcard
learnerAnswersCorrectly (LearnerFlashcard {flashcard = flashcard, score = score}) = 
  LearnerFlashcard {flashcard =  flashcard, score = 1 + score}
