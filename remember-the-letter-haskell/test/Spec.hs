{-|
Module      : Spec
Description : Automated test entry-point.
-}

import LessonTests (lessonSpecs)
import Test.Hspec

main :: IO ()
main = hspec lessonSpecs
