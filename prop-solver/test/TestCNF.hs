module TestCNF (tests) where

import Test.HUnit
import Cnf
import Propositional (Prop(..))

pImpliesQ :: Prop
pImpliesQ = Imply (Var 'P') (Var 'Q')

notPOrQ :: Prop
notPOrQ = Or (Not (Var 'P')) (Var 'Q')

elimImp1 :: Test
elimImp1 = TestCase(assertEqual "Eliminate Implication Test 1" (elimImp pImpliesQ) notPOrQ)


tests :: Test
tests = TestList [elimImp1]