{-|
Module      : Spec
Description : Automated test entry-point.
-}

import Prelude (IO)
import LessonTests (lessonSpecs)
import LearnerLessonTests (learnerLessonSpecs)
import Test.Hspec (hspec)

main :: IO ()
main = 
  do
  hspec lessonSpecs
  hspec learnerLessonSpecs
