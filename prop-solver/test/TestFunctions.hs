module TestFunctions (tests) where

import Test.HUnit
import Functions
import Propositional (Prop(..))

-- Define expected value
expectedValue :: Prop
expectedValue = Not (Var 'A')

expectedValue2 :: Prop
expectedValue2 = Not (And (Var 'A') (Var 'B'))

testValue2 :: Prop
testValue2 = Imply (Var 'A') (Var 'B')

testValue3 :: Prop
testValue3 = Or (Var 'B') (Var 'A')

expectedValue3 :: Prop
expectedValue3 = And (Var 'A') (Var 'B')

expectedValue4 :: Prop
expectedValue4 = Var 'A'

testValue1 :: Prop
testValue1 = Or (Var 'A') (Var 'B')

testValue1 :: Prop
testValue1 = Or (Var 'A') (Var 'B')

-- Test cases
negation1 :: Test
negation1 = TestCase (assertEqual "Negation Test 1" (negation (Var 'A')) expectedValue)

negation2 :: Test
negation2 = TestCase (assertEqual "Negation Test 2" (negation (And (Var 'A') (Var 'B'))) expectedValue2)

conjInt1 :: Test
conjInt1 = TestCase (assertEqual "Conjunction Introduction Test 1" (conjInt (Var 'A') (Var 'B')) (Just expectedValue3))

conjElimL1 :: Test
conjElimL1 = TestCase (assertEqual "Conjunction Elimination Test 1" (conjElimL expectedValue3) (Just expectedValue4))

conjElimL2 :: Test
conjElimL2 = TestCase (assertEqual "Conjunction Elimination Test 2" (conjElimL testValue1) (Nothing))

conjElimR1 :: Test
conjElimR1 = TestCase (assertEqual "Conjunction Elimination Test 3" (conjElimR expectedValue3) (Just expectedValue5))

conjElimR2 :: Test
conjElimR2 = TestCase (assertEqual "Conjunction Elimination Test 3" (conjElimR testValue2) (Nothing))

impElim1 :: Test
impElim1 = TestCase (assertEqual "Implication Elimination Test 1" (impElim testValue2 expectedValue4) (Just expectedValue5))

impElim2 :: Test
impElim2 = TestCase (assertEqual "Implication Elimination Test 2" (impElim testValue2 expectedValue5) (Nothing))

impElim3 :: Test
impElim3 = TestCase (assertEqual "Implication Elimination Test 3" (impElim testValue1 expectedValue5) (Nothing))

disjIntL1 :: Test
disjIntL1 = TestCase (assertEqual "Disjunction Introduction Test 1" (disjIntL expectedValue4 expectedValue5) (Just testValue1))

disjIntR1 :: Test
disjIntR1 = TestCase (assertEqual "Disjunction Introduction Test 2" (disjIntL expectedValue5 expectedValue4) (Just testValue3))

impInt1 :: Test
impInt1 = TestCase (assertEqual "Implication Introduction Test 1" (impInt expectedValue4 expectedValue5) (testValue2))

-- Test list
tests :: Test
tests = TestList [negation1,negation2, conjInt1, conjElimL1, conjElimL2, conjElimR1, conjElimR2, impElim1, impElim2, impElim3, disjIntL1, disjIntR1, impInt1]