{-|
Module      : Spec
Description : Automated test entry-point.
-}

import Prelude (IO)
import LessonTests (lessonSpecs)
import Test.Hspec (hspec)

main :: IO ()
main = hspec lessonSpecs
