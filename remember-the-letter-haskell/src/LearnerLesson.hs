{-|
Module      : LearnerLesson
Description : User data with respect to a lesson. A lesson consists of flashcard data, and related functions.
-}
module LearnerLesson where

import Prelude ((+), Show, Eq, Int)

import Types.Flashcard(Flashcard)
import Types.LearnerFlashcard(LearnerFlashcard(LearnerFlashcard), flashcard, correctCount, attemptCount)


-- | The function that given a flashcard (and learner-flashcard statistics) returns the flashcard
--   after its learner recalls that flashcard correctly.
learnerAnswersCorrectly :: LearnerFlashcard -- ^ a flashcard with statistics
                        -> LearnerFlashcard -- ^ the flashcard with statistics after a correct answer
learnerAnswersCorrectly c =
  LearnerFlashcard
  { flashcard =  flashcard c
  , correctCount = 1 + correctCount c
  , attemptCount = 1 + attemptCount c
  }

-- | The function that given a flashcard (and learner-flashcard statistics) returns the flashcard
--   after its learner fails to recall that flashcard.
learnerAnswersIncorrectly :: LearnerFlashcard -- ^ a flashcard with statistics
                          -> LearnerFlashcard -- ^ the flashcard with statistics after an incorrect answer
learnerAnswersIncorrectly c =
  LearnerFlashcard
  { flashcard =  flashcard c
  , correctCount = correctCount c
  , attemptCount = 1 + attemptCount c
  }
