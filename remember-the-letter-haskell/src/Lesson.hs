{-|
Module      : Lesson
Description : A lesson consists of flashcard data, and related functions.
-}


module Lesson where

-- | A flahscard has a front side, and a back side.
data Flashcard =
  Flashcard
  { front :: String
  , back  :: String
  }



-- | The front and backs of every flashcard in a given list.
lessonSummary :: [Flashcard] -> String
lessonSummary flashcards = unlines (map showFlashcard flashcards)

-- | The fronts of every flashcard in a given list.
frontSummary :: [Flashcard] -> String
frontSummary flashcards = unlines (map showFlashcardFront flashcards)

-- | The front and back of a single flashcard.
showFlashcard :: Flashcard -> String
showFlashcard card = (front card) ++ " | " ++ (back card)

-- | Show the front of a single flashcard
showFlashcardFront :: Flashcard -> String
showFlashcardFront card = front card
