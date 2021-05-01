-- Some examples
eval Stop [] [];
-- []
eval (Tell (Constraint "x" 3 4)) [] [];
-- [("x",3,4)]
eval (Ask (Constraint "x" 2 4) (Tell (Constraint "x" 3 4))) [] [];
-- Blocking
eval (Ask (Constraint "x" 2 4) (Tell (Constraint "x" 3 4))) [(Constraint "x" 2 4)] [];
-- [("x",5,8)]
eval (ProcCall "p" "z") [(Constraint "z" 2 4)] [("p", ("x", Tell (Constraint"x" 3 5)))];
-- [("z",5,9)]
let a1 = Tell (Constraint "x" 2 4)
let a2 = Ask (Constraint "x" 2 4) (Tell (Constraint "x" 3 4))
eval (Par(a1, a2)) [] []
-- [("x",5,8)]