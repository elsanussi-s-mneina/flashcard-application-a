{-|
Module      : Flashcard
Description : A flashcard contains text data divided into two sides.
-}


module Flashcard where

import Prelude ((==), (/=), (++), Bool, Char, Eq, Show, String, break, filter)

-- | A flashcard has a front side, and a back side.
-- The front of the flashcard contains text.
-- The back of the flashcard contains text.
data Flashcard =
  Flashcard
  { front :: String
  , back  :: String
  } deriving (Eq, Show)


-- | The front and back of a single flashcard.
-- Specifically:
-- Text of the front of a flashcard then a separator then the back of a flashcard
show :: Flashcard -- ^ a flashcard
     -> String    -- ^ text contained on the front and back of the flashcard
show card = front card ++ " | " ++ back card

-- | Show the front of a single flashcard.
showFront :: Flashcard -- ^ a flashcard
                   -> String    -- ^ text contained on the front of the flashcard
showFront = front

-- | Show the back of a single flashcard.
showBack :: Flashcard -- ^ a flashcard
                  -> String    -- ^ text contained on the back of the flashcard
showBack = back

-- | Convert a flashcard to tab separated values.
-- The front is first,
-- then a tab character as a separator,
-- then the back, then a new line character.
toTabSeparatedValues :: Flashcard -- ^ a flashcard
                     -> String    -- ^ a line of text containing the front of the flashcard, then
                                           --   a tab character, then the back of the flashcard, and then
                                           --   a new line character.
toTabSeparatedValues (Flashcard f b) = f ++ "\t" ++ b ++ "\n"

-- | Convert tab separated values (a single line) to a flashcard.
fromTabSeparatedValues :: String -- ^ a line of text containing the front of a flashcard,
                                 --   then a tab character, then the back of the flashcard,
                                 --   then a new line character.
                       -> Flashcard -- ^ a flashcard
fromTabSeparatedValues line =
  let line' = filter (/= '\n') line
      (front, back) = splitOn '\t' line'
  in Flashcard front back

-- | Given a separation character, and text, it splits it into
--   two pieces at the separation character. The separation character
--   is not included in the parts.
splitOn :: Char             -- ^ separation character
        -> String           -- ^ text (likely with separation character in the middle)
        -> (String, String) -- ^ the front of the card then the back of the card (as two parts)
splitOn character text =
  let (ff, backWithTab) =  break (== character) text
  in (ff, filter (/= character) backWithTab)


-- | surrounding a string with double quotes
doubleQuote :: String -- ^ some text
            -> String -- ^ the same text but with two extra characters surrounding it, both double quote characters.
doubleQuote s = "\"" ++ s ++ "\""

-- | A message telling the user
--   what the back of the current flashcard is.
--   This may be used in a quiz.
presentBack :: Flashcard -- ^ a flashcard
                       -> String    -- ^ text declaring the back of a flashcard to the user,
                                    --   in a clear manner.
presentBack fla = "I am showing you the back of a flashcard.\nYou see " ++ doubleQuote (showBack fla)

-- | A message telling the user
--   what the front of the current flashcard is.
--   This may be used in a quiz.
presentFront :: Flashcard -- ^ a flashcard
             -> String    -- ^ text declaring the front of a flashcard to the user,
                          --   in a clear manner.
presentFront fla = "I am showing you the front of a flashcard.\nYou see " ++ doubleQuote (showFront fla)

-- |  Given user input, and a flashcard
--    judge whether the user input is the same
--    as the back of the flashcard.
checkUserAttemptToProvideBack :: Flashcard -- ^ a flashcard
                              -> String    -- ^ user's guess of what the back of the flashcard is
                              -> Bool      -- ^ whether the guess is correct
checkUserAttemptToProvideBack fla userAttempt = userAttempt == back fla

-- | Given user input, and a flashcard
--   judge whether the user input is the same
--   as the front of the flashcard.
checkUserAttemptToProvideFront :: Flashcard -- ^ a flashcard
                               -> String    -- ^ user's guess of what the front of the flashcard is
                               -> Bool      -- ^ whether the guess is correct
checkUserAttemptToProvideFront fla userAttempt = userAttempt == front fla