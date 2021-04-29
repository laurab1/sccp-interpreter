{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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