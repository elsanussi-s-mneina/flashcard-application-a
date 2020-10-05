{-|
Module      : Flashcard
Description : A flashcard contains text data divided into two sides.
-}
module Types.Flashcard where

import Prelude (Eq, Show, String)

-- | A flashcard has a front side, and a back side.
-- The front of the flashcard contains text.
-- The back of the flashcard contains text.
data Flashcard =
  Flashcard
  { front :: String
  , back  :: String
  } deriving (Eq, Show)
