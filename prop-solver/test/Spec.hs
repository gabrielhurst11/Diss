import Test.HUnit
import TestFunctions
import TestCNF

main :: IO ()
main = do
  _ <- runTestTT TestFunctions.tests
  pure ()
  _ <- runTestTT TestCNF.tests
  pure ()