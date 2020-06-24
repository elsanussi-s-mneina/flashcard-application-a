{-|
Module      : LearnerLesson
Description : User data with respect to a lesson. A lesson consists of flashcard data, and related functions.
-}
module LearnerLesson where

import Lesson(Flashcard(Flashcard), back, front)

-- | A flashcard with user specific information.
data LearnerFlashcard = LearnerFlashcard
  { flashcard :: Flashcard   -- ^ a flashcard
  , correctCount :: Int      -- ^ the number of correct guesses the user made
  , attemptCount :: Int      -- ^ the number of guesses both correct or incorrect the user made
  } 
  deriving (Show, Eq)

-- | The function that given a flashcard (and learner-flashcard statistics) returns the flashcard
--   after its learner recalls that flashcard correctly.
learnerAnswersCorrectly :: LearnerFlashcard -- ^ a flashcard with statistics
                        -> LearnerFlashcard -- ^ the flashcard with statistics after a correct answer
learnerAnswersCorrectly (LearnerFlashcard {flashcard = f, correctCount = cc, attemptCount = ac}) = 
  LearnerFlashcard {flashcard =  f, correctCount = 1 + cc, attemptCount = 1 + ac}

-- | The function that given a flashcard (and learner-flashcard statistics) returns the flashcard
--   after its learner fails to recall that flashcard.
learnerAnswersIncorrectly :: LearnerFlashcard -- ^ a flashcard with statistics
                          -> LearnerFlashcard -- ^ the flashcard with statistics after an incorrect answer
learnerAnswersIncorrectly (LearnerFlashcard {flashcard = f, correctCount = cc, attemptCount = ac}) = 
  LearnerFlashcard {flashcard =  f, correctCount = cc, attemptCount = ac + 1}
