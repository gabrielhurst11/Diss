module TestCNF (tests) where

import Test.HUnit
import Cnf
import Propositional (Prop(..))

pImpliesQ :: Prop
pImpliesQ = Imply (Var 'P') (Var 'Q')

notPOrQ :: Prop
notPOrQ = Or (Not (Var 'P')) (Var 'Q')

notPAndQ :: Prop
notPAndQ = (Not (And (Var 'P') (Var 'Q')))

notPOrNotQ :: Prop
notPOrNotQ = Or (Not (Var 'P')) (Not (Var 'Q'))

pAndQ_Or_POrR :: Prop
pAndQ_Or_POrR = Or (And (Var 'P') (Var 'Q')) (Or (Var 'Q') (Var 'R')) 

pOrQOrR_And_QOrQOrR :: Prop
pOrQOrR_And_QOrQOrR = And (Or (Var 'P') (Or (Var 'Q') (Var 'R'))) (Or (Var 'Q') (Or (Var 'Q') (Var 'R')))

elimImp1 :: Test
elimImp1 = TestCase(assertEqual "Eliminate Implication Test 1" (elimImp pImpliesQ) notPOrQ)

pushNegation1 :: Test
pushNegation1 = TestCase(assertEqual "Push Negation Test 1" (pushNegation notPAndQ) notPOrNotQ)

distribute1 :: Test
distribute1 = TestCase(assertEqual "Distribution Test 1" (distribute pAndQ_Or_POrR) pOrQOrR_And_QOrQOrR)


tests :: Test
tests = TestList [elimImp1, pushNegation1, distribute1]