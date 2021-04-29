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
import Defs
import Types
import Control.Parallel

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
    Hide (v, a1) ->
        if acontains a1 v then eval (subst a1 v) d else eval a1 d