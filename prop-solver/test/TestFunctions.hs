module TestFunctions (tests) where

import Test.HUnit
import Functions
import Propositional (Prop(..))

-- Define expected value
notA :: Prop
notA = Not (Var 'A')

notAAndB :: Prop
notAAndB = Not (And (Var 'A') (Var 'B'))

aImpliesB :: Prop
aImpliesB = Imply (Var 'A') (Var 'B')

bOrA :: Prop
bOrA = Or (Var 'B') (Var 'A')

aAndB :: Prop
aAndB = And (Var 'A') (Var 'B')

varA :: Prop
varA = Var 'A'

varB :: Prop
varB = Var 'B'

aOrB:: Prop
aOrB = Or (Var 'A') (Var 'B')


-- Test cases
negation1 :: Test
negation1 = TestCase (assertEqual "Negation Test 1" (negation (Var 'A')) notA)

negation2 :: Test
negation2 = TestCase (assertEqual "Negation Test 2" (negation (And (Var 'A') (Var 'B'))) notAAndB)

conjInt1 :: Test
conjInt1 = TestCase (assertEqual "Conjunction Introduction Test 1" (conjInt (Var 'A') (Var 'B')) (Just aAndB))

conjElimL1 :: Test
conjElimL1 = TestCase (assertEqual "Conjunction Elimination Test 1" (conjElimL aAndB) (Just varA))

conjElimL2 :: Test
conjElimL2 = TestCase (assertEqual "Conjunction Elimination Test 2" (conjElimL aOrB) (Nothing))

conjElimR1 :: Test
conjElimR1 = TestCase (assertEqual "Conjunction Elimination Test 3" (conjElimR aAndB) (Just varB))

conjElimR2 :: Test
conjElimR2 = TestCase (assertEqual "Conjunction Elimination Test 3" (conjElimR aImpliesB) (Nothing))

impElim1 :: Test
impElim1 = TestCase (assertEqual "Implication Elimination Test 1" (impElim aImpliesB varA) (Just varB))

impElim2 :: Test
impElim2 = TestCase (assertEqual "Implication Elimination Test 2" (impElim aImpliesB varB) (Nothing))

impElim3 :: Test
impElim3 = TestCase (assertEqual "Implication Elimination Test 3" (impElim aOrB varB) (Nothing))

disjIntL1 :: Test
disjIntL1 = TestCase (assertEqual "Disjunction Introduction Test 1" (disjIntL varA varB) (Just aOrB))

disjIntR1 :: Test
disjIntR1 = TestCase (assertEqual "Disjunction Introduction Test 2" (disjIntR varA varB) (Just bOrA))

impInt1 :: Test
impInt1 = TestCase (assertEqual "Implication Introduction Test 1" (impInt varA varB) (Just aImpliesB))

-- Test list
tests :: Test
tests = TestList [negation1,negation2, conjInt1, conjElimL1, conjElimL2, conjElimR1, conjElimR2, impElim1, impElim2, impElim3, disjIntL1, disjIntR1, impInt1]