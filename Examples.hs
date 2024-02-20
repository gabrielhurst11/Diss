module Examples where

import Propositional (Prop(..))

-- A and Not A
p1 :: Prop 
p1 = And (Var 'A')(Not(Var 'A'))

-- A and B implies A
p2 :: Prop
p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')

-- A implies A and B
p3 :: Prop
p3 = Imply (Var 'A') (And (Var 'A') (Var 'B'))

-- (A and A implies B) implies B
p4 :: Prop
p4 = Imply (And (Var 'A') (Imply (Var 'A') (Var 'B'))) (Var 'B')


-- (A and B implies B and C)
p5 :: Prop
p5 = Imply (And (Var 'A') (Var 'B')) (And (Var 'B') (Var 'C'))

-- A or B
p6 :: Prop
p6 = Or (Var 'A') (Var 'B')

conjCheck :: Prop
conjCheck = And (Var 'A') (Var 'B')

impCheck1 :: Prop
impCheck1 = Imply (Var 'A') (Var 'B')

impCheck2 :: Prop
impCheck2 = (Var 'A')

disjTest :: Prop
disjTest = Var 'A'