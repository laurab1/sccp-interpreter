--Copyright 2021 Laura Bussi

--   Licensed under the Apache License, Version 2.0 (the "License");
--   you may not use this file except in compliance with the License.
--   You may obtain a copy of the License at

--       http://www.apache.org/licenses/LICENSE-2.0

--   Unless required by applicable law or agreed to in writing, software
--   distributed under the License is distributed on an "AS IS" BASIS,
--   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--   See the License for the specific language governing permissions and
--   limitations under the License.

-- Contains the agents definitions and some auxiliary functions
-- needed to perform substitutions of variables, creating fresh
-- variables and manipulating the store.
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
module Defs (
    module Defs
) where

import Types
import Data.Char

type Delta = [(Constraint Var Int Int)]

type IntC = (Constraint Var Int Int)

data Agent = Tell IntC | Ask IntC Agent | Stop
    | Par (Agent, Agent) | Hide (Var, Agent) | ProcCall Var Var 
    deriving Show

type Closure = (Var, Agent)

type Env = [(Var, Closure)]


cappend (Constraint a b c) EmptyC = Constraint a b c 
cappend EmptyC (Constraint a b c) = Constraint a b c
cappend (Constraint a1 b1 c1) (Constraint a2 b2 c2) = 
    if a1 == a2 then (Constraint a1 (b1 + b2) (c1 + c2)) else EmptyC

instance (Num c, Num b) => Semigroup (Constraint a b c) where
    (<>) = cappend

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

dcontains d x = case d of
    [] -> False
    (Constraint y b c):cs -> if x == y then True else dcontains cs x

acontains a x env = case a of
    Stop -> False
    Tell (Constraint y b c) -> if x == y then True else False
    Ask (Constraint y b c) a1 -> if x == y then True else acontains a1 x env
    Par (a1, a2) -> (acontains a1 x env) || (acontains a2 x env)
    Hide (y, a1) -> if x == y then False else (acontains a1 x env)
    ProcCall p w -> let res = lookup p env in
        case res of
            Nothing -> error ("undefined identifier " ++ p)
            Just (var, cl) -> if var == x then True else acontains cl x env

freshx x d = 
    let newx = (if length x == 1 then x ++ "1" else x ++ [intToDigit ((digitToInt (x!!1)) + 1)])
        in aux newx d where
            aux z d1 = if dcontains d1 z then z else freshx z d1

subst a x z = case a of
    Stop -> Stop
    Tell c1 @ (Constraint y b c) -> 
        if x == y then Tell (Constraint z b c) else (Tell c1)
    Ask c1 @ (Constraint y b c) a1 -> 
        if x == y then Ask (Constraint z b c) (subst a1 x z) else (Ask c1 (subst a1 x z))
    Hide (y, a1) -> 
        if x == y then Hide (z, a1) else
            if y == z then Hide ((freshx z []), (subst (subst a1 x z) z (freshx z [])))
            else Hide (y, (subst a1 x z))
    Par (a1, a2) -> Par ((subst a1 x z), (subst a2 x z))
    ProcCall p w -> if w == x then ProcCall p z else ProcCall p w 