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

-- Main interpreter
module Eval (
    eval
) where

import Defs
import Types
import Control.Parallel

eval a d r = case a of
    Stop -> d
    Tell (Constraint x b c) -> tell (Constraint x b c) d
    Ask (Constraint x b c) a1 ->
        if ask (Constraint x b c) d
            then eval a1 d r
            else eval a d r
    Par (a1, a2) -> 
        (par e1 e2) where
            e1 = eval a1 d r
            e2 = eval a2 e1 r
    Hide (v, a1) ->
        if acontains a1 v r then eval (subst a v (freshx v d)) d r else eval a1 d r
    ProcCall p y -> let closure = lookup p r in
        case closure of
            Nothing -> error ("undefined identifier " ++ p)
            Just (var, aloc) ->
                let anew = subst aloc var y in
                    eval anew d r