{-|
Module      : Lesson
Description : A lesson consists of flashcard data, and related functions.
-}


module Lesson where

import Types.Flashcard (Flashcard(Flashcard))
import qualified Flashcard

-- | This function adds a flashcard to a lesson.
-- This function works by returning the lesson with the new flashcard added.
addFlashcard :: [Flashcard] -- ^ a list of flashcards
             -> String      -- ^ front side of new flashcard
             -> String      -- ^ back side of new flashcard
             -> [Flashcard] -- ^ a list of flashcards with a new flashcard
addFlashcard flashcards front back =
  flashcards ++ [Flashcard front back]

-- | Text that summarizes flashcard in a given list.
-- It shows the summary of one flashcard per line.
summary :: [Flashcard] -- ^ a list of flashcards
        -> String      -- ^ text containing front and back of each flashcard
summary flashcards = unlines (map Flashcard.show flashcards)

-- | The front of every flashcard in a given list.
-- It shows the front of one flashcard per line.
frontSummary :: [Flashcard]   -- ^ list of flashcards
             -> String        -- ^ front of a card then a new line
                              --   then the front of the next card, and so on.
frontSummary flashcards = unlines (map Flashcard.showFront flashcards)

-- | The backs of every flashcard in a given list.
-- It shows the back of one flashcard per line.
backSummary :: [Flashcard]   -- ^ list of flashcards
            -> String        -- ^ back of a card then a new line
                             --   then the back of the next card, and so on
backSummary flashcards = unlines (map Flashcard.showBack flashcards)

-- | Convert the flashcards to a tab separated values format.
toTabSeparatedValues :: [Flashcard] -- ^  list of flashcards
                     -> String      -- ^  plain text containing details of every flashcard
                                    --    on a separate line, in tab separated values format.
toTabSeparatedValues =
  concatMap Flashcard.toTabSeparatedValues

-- | Convert tab separated values (multiple lines) to flashcards.
fromTabSeparatedValues :: String      -- ^ plain text containing details of every flashcard on a separate line,
                                      --   in tab separated values format.
                       -> [Flashcard] -- ^ a list of flashcards
fromTabSeparatedValues contents =
  map Flashcard.fromTabSeparatedValues (lines contents)

-- |  Given user input, and a flashcard
--    judge whether the user input is the same
--    as the back of the flashcard.
checkUserAttemptToProvideBack :: Flashcard -- ^ a flashcard
                              -> String    -- ^ user's guess of what the back of the flashcard is
                              -> Bool      -- ^ whether the guess is correct
checkUserAttemptToProvideBack fla userAttempt = userAttempt == Flashcard.back fla

-- | Given user input, and a flashcard
--   judge whether the user input is the same
--   as the front of the flashcard.
checkUserAttemptToProvideFront :: Flashcard -- ^ a flashcard
                               -> String    -- ^ user's guess of what the front of the flashcard is
                               -> Bool      -- ^ whether the guess is correct
checkUserAttemptToProvideFront fla userAttempt = userAttempt == Flashcard.front fla