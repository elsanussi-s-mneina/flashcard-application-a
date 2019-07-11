module Lesson
    ( lessonSummary
    ) where


lessonSummary :: String
lessonSummary =  showFlashcard flashcard1 ++ showFlashcard flashcard2

showFlashcard :: String -> String
showFlashcard x = x ++ " \n "

flashcard1 :: String
flashcard1 = "the | le/la"

flashcard2 :: String
flashcard2 = "a | un/une"
