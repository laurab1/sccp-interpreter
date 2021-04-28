{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Data.Monoid
import System.IO

class Monoid a => ResOrderedMonoid a where
    msubtract :: a -> a -> a
    (<=) :: a -> a -> Bool

type Var = Char

data Constraint x d v where
    Constraint :: (Eq v, Show v) => Var -> Maybe d -> v -> Constraint x d v1

instance Show (Constraint a b c) where
    show (Constraint a b c) = case b of
        Nothing -> show "empty constraint"
        Just val -> show c

eval (Constraint x1 d1 v1) = Constraint x1 d1 v1
