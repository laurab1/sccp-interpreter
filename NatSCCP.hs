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

contains d x = case d of
    [] -> False
    (Constraint y b c):cs -> if x == y then True else contains cs x 

eval a d = case a of
    Stop -> d
    Tell (Constraint x b c) -> tell (Constraint x b c) d
    Ask (Constraint x b c) a1 ->
        if ask (Constraint x b c) d
            then eval a1 d
            else eval a d
    Par (a1, a2) -> 
        (par e1 (pseq e2 (merge e1 e2))) where
            e1 = eval a1 d
            e2 = eval a2 d
            merge d1 d2 = case d1 of
                [] -> d2
                c:cs -> merge cs (tell c d2)