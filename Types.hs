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

-- Contains the definitions of a residuated, partially ordered monoid
-- and of a (soft) constraint.
{-# LANGUAGE GADTs #-}
module Types (
    module Types
) where

import Data.Monoid

class Monoid a => RPOMonoid a where
    msubtract :: a -> a -> a
    mleq :: a -> a -> Bool

type Var = [Char]

data Constraint x d v where
    Constraint :: (Eq v, Show v, Show d) => Var -> d -> v -> Constraint x d v
    EmptyC :: Constraint x d v

instance Show (Constraint a b c) where
    show (Constraint a b c) = show (a, b, c)