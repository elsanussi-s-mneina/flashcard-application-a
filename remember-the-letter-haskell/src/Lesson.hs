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
  } deriving (Eq, Show)



-- | The front and backs of every flashcard in a given list.
lessonSummary :: [Flashcard] -> String
lessonSummary flashcards = unlines (map showFlashcard flashcards)

-- | The front of every flashcard in a given list.
frontSummary :: [Flashcard]   -- ^ list of flashcards
             -> String        -- ^ front of a card then a new line
                              --   then the front of the next card, and so on.
frontSummary flashcards = unlines (map showFlashcardFront flashcards)

-- | The backs of every flashcard in a given list.
backSummary :: [Flashcard]   -- ^ list of flashcards
            -> String        -- ^ back of a card then a new line
                             --   then the back of the next card, and so on
backSummary flashcards = unlines (map showFlashcardBack flashcards)

-- | The front and back of a single flashcard.
showFlashcard :: Flashcard -> String
showFlashcard card = (front card) ++ " | " ++ (back card)

-- | Show the front of a single flashcard.
showFlashcardFront :: Flashcard -> String
showFlashcardFront card = front card

-- | Show the back of a single flashcard.
showFlashcardBack :: Flashcard -> String
showFlashcardBack card = back card

-- | Convert a flashcard to tab separated values.
tabSeparatedValuesOfFlashcard :: Flashcard -> String
tabSeparatedValuesOfFlashcard (Flashcard f b) = f ++ "\t" ++ b ++ "\n"

-- | Convert tab separated values (a single line) to a flashcard.
tabSeparatedValuesToFlashcard :: String -> Flashcard
tabSeparatedValuesToFlashcard line =
  let line' = filter (/= '\n') line
      (front, backWithTabAtFront) = break (=='\t') line'
      back = (filter (/= '\t') backWithTabAtFront)
  in (Flashcard front back)
  -- This should be refactored.


-- | Convert the flashcards to a tab separated values format.
tabSeparatedValuesOfLesson :: [Flashcard] -> String
tabSeparatedValuesOfLesson flashcards =
  foldl (++) "" (map tabSeparatedValuesOfFlashcard flashcards)

-- | Convert tab separated values (multiple lines) to flashcards.
tabSeparatedValuesToLesson :: String -> [Flashcard]
tabSeparatedValuesToLesson contents =
  map tabSeparatedValuesToFlashcard (lines contents)
