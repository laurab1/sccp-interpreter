import Types

type Delta = [(Constraint Var Int Int)]

type IntC = (Constraint Var Int Int)

data Agent = Tell IntC | Ask IntC Agent 
    | Stop | Par (Agent, Agent) | Ex (Var, Agent) | ProcCall Var 
    deriving Show

cappend (Constraint a b c) EmptyC = Constraint a b c 
cappend EmptyC (Constraint a b c) = Constraint a b c
cappend (Constraint a1 b1 c1) (Constraint a2 b2 c2) = 
    if a1 == a2 then (Constraint a1 (b1 + b2) (c1 + c2)) else EmptyC

instance (Num c, Num b) => Monoid (Constraint a b c) where
    mempty = EmptyC
    mappend = cappend

csubtract (Constraint a b c) EmptyC = Constraint a b c
csubtract EmptyC (Constraint a b c) = EmptyC
csubtract (Constraint a1 b1 c1) (Constraint a2 b2 c2) =
    if a1 == a2 then (Constraint a1 (max (b1 - b2) 0) (max c1 c2)) else EmptyC

cleq (Constraint a1 b1 c1) (Constraint a2 b2 c2) = 
    if b1 <= b2 then (Constraint a1 b1 c1) else (Constraint a2 b2 c2) 
cleq (Constraint a1 b1 c1) EmptyC = EmptyC
cleq EmptyC (Constraint a1 b1 c1) = EmptyC

instance (Ord c, Ord b, Num c, Num b) => RPOMonoid (Constraint a b c) where
    msubtract = csubtract
    mleq = cleq