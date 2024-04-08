module TestFunctions (tests) where

import Test.HUnit
import Functions
import Propositional (Prop(..))

-- Define expected value
expectedValue :: Prop
expectedValue = Not (Var 'A')

-- Test cases
test1 :: Test
test1 = TestCase (assertEqual "Negation Test 1" (negation (Var 'A')) expectedValue)

-- Define expected value
expectedValue2 :: Prop
expectedValue2 = Not (And (Var 'A') (Var 'B'))

test2 :: Test
test2 = TestCase (assertEqual "Negation Test 1" (negation (And (Var 'A') (Var 'B'))) expectedValue2)

-- Test list
tests :: Test
tests = TestList [test1,test2]