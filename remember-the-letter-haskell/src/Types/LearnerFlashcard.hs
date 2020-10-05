{-|
Module      : LearnerFlashcard
Description : A flashcard, and user related statistics for that flashcard.
-}
module Types.LearnerFlashcard where

import Prelude ((+), Show, Eq, Int)

import Types.Flashcard(Flashcard)

-- | A flashcard with user specific information.
data LearnerFlashcard = LearnerFlashcard
  { flashcard :: Flashcard   -- ^ a flashcard
  , correctCount :: Int      -- ^ the number of correct guesses the user made
  , attemptCount :: Int      -- ^ the number of guesses both correct or incorrect the user made
  } 
  deriving (Show, Eq)