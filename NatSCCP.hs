import Types
import Control.Parallel

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
    if b1 <= b2 then True else False
cleq (Constraint a1 b1 c1) EmptyC = False
cleq EmptyC (Constraint a1 b1 c1) = True

instance (Ord c, Ord b, Num c, Num b) => RPOMonoid (Constraint a b c) where
    msubtract = csubtract
    mleq = cleq

tell (Constraint a b c) delta = case delta of
    [] -> [(Constraint a b c)]
    (Constraint a1 b1 c1):cs -> 
        if a1 == a then (mappend (Constraint a b c) (Constraint a1 b1 c1)):cs
        else tell (Constraint a b c) cs

ask (Constraint a b c) delta = case delta of
    [] -> False
    (Constraint a1 b1 c1):cs ->
        if a == a1 then 
            if b == b1 then True
            else ask (Constraint a b c) cs
        else ask (Constraint a b c) cs

-- naive
eval a d = case a of
    Stop -> d
    Tell (Constraint a b c) ->  tell (Constraint a b c) d
    Ask (Constraint a b c) a1 -> if (ask (Constraint a b c) d) then (eval a1 d) else d
    Par (a, b) -> 
        par e1 (pseq e1 (e1 ++ e2)) where
            e1 = (eval a d)
            e2 = (eval b d)