import Test.HUnit
import TestFunctions

main :: IO ()
main = do
  _ <- runTestTT TestFunctions.tests
  pure ()