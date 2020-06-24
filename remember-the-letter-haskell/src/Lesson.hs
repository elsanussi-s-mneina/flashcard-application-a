{-|
Module      : Lesson
Description : A lesson consists of flashcard data, and related functions.
-}


module Lesson where


-- | A flashcard has a front side, and a back side.
data Flashcard =
  Flashcard
  { front :: String
  , back  :: String
  } deriving (Eq, Show)


-- | This function adds a flashcard to a lesson.
-- This function works by returning the lesson with the new flashcard added.
addFlashcardToLesson :: [Flashcard] -- ^ a list of flashcards
                     -> String      -- ^ front side of new flashcard
                     -> String      -- ^ back side of new flashcard
                     -> [Flashcard] -- ^ a list of flashcards with a new flashcard
addFlashcardToLesson flashcards front back =
  flashcards ++ [(Flashcard front back)]

-- | The front and backs of every flashcard in a given list.
lessonSummary :: [Flashcard] -- ^ a list of flashcards
              -> String      -- ^ text containing front and back of each flashcard
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
showFlashcard :: Flashcard -- ^ a flashcard
              -> String    -- ^ text contained on the front and back of the flashcard
showFlashcard card = front card ++ " | " ++ back card

-- | Show the front of a single flashcard.
showFlashcardFront :: Flashcard -- ^ a flashcard
                   -> String    -- ^ text contained on the front of the flashcard
showFlashcardFront = front

-- | Show the back of a single flashcard.
showFlashcardBack :: Flashcard -- ^ a flashcard
                  -> String    -- ^ text contained on the back of the flashcard
showFlashcardBack = back

-- | Convert a flashcard to tab separated values.
tabSeparatedValuesOfFlashcard :: Flashcard -- ^ a flashcard
                              -> String    -- ^ a line of text containing the front of the flashcard, then
                                           --   a tab character, then the back of the flashcard, and then
                                           --   a new line character.   
tabSeparatedValuesOfFlashcard (Flashcard f b) = f ++ "\t" ++ b ++ "\n"

-- | Convert tab separated values (a single line) to a flashcard.
tabSeparatedValuesToFlashcard :: String -- ^ a line of text containing the front of a flashcard,     
                                        --   then a tab character, then the back of the flashcard,
                                        --   then a new line character.
                              -> Flashcard -- ^ a flashcard
tabSeparatedValuesToFlashcard line =
  let line' = filter (/= '\n') line
      (front, backWithTabAtFront) = break (=='\t') line'
      back = filter (/= '\t') backWithTabAtFront
  in Flashcard front back
  -- This should be refactored.


-- | Convert the flashcards to a tab separated values format.
tabSeparatedValuesOfLesson :: [Flashcard] -- ^  list of flashcards
                           -> String      -- ^  plain text containing details of every flashcard
                                          --    on a separate line, in tab separated values format.  
tabSeparatedValuesOfLesson =
  concatMap tabSeparatedValuesOfFlashcard

-- | Convert tab separated values (multiple lines) to flashcards.
tabSeparatedValuesToLesson :: String      -- ^ plain text containing details of every flashcard on a separate line,
--                                        --   in tab separated values format.
                           -> [Flashcard] -- ^ a list of flashcards
tabSeparatedValuesToLesson contents =
  map tabSeparatedValuesToFlashcard (lines contents)

-- | surrounding a string with double quotes
doubleQuote :: String -- ^ some text
            -> String -- ^ the same text but with two extra characters surrounding it, both double quote characters.
doubleQuote s = "\"" ++ s ++ "\""

presentBackOfFlashcard :: Flashcard -- ^ a flashcard
                       -> String    -- ^ text declaring the back of a flashcard to the user,
                                    --   in a clear manner.  
presentBackOfFlashcard fla = "I am showing you the back of a flashcard.\nYou see " ++ doubleQuote (showFlashcardBack fla)

presentFrontOfFlashcard :: Flashcard -- ^ a flashcard
                        -> String    -- ^ text declaring the front of a flashcard to the user,
                                     --   in a clear manner.     
presentFrontOfFlashcard fla = "I am showing you the front of a flashcard.\nYou see " ++ doubleQuote (showFlashcardFront fla)

checkUserAttemptToProvideBack :: Flashcard -- ^ a flashcard
                              -> String    -- ^ user's guess of what the back of the flashcard is
                              -> Bool      -- ^ whether the guess is correct
checkUserAttemptToProvideBack fla userAttempt = userAttempt == back fla

checkUserAttemptToProvideFront :: Flashcard -- ^ a flashcard
                               -> String    -- ^ user's guess of what the front of the flashcard is
                               -> Bool      -- ^ whether the guess is correct
checkUserAttemptToProvideFront fla userAttempt = userAttempt == front fla