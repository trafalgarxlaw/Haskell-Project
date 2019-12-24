{-|
Module      : InfInt
Description : A module for integer enriched with infinite values
Copyright   : (c) Alexandre Blondin Massé
License     : GPL-3
Maintainer  : blondin_masse.alexandre@uqam.ca
Stability   : experimental

This module provides functionalities for handling integer values, enriched with
positive and negative infinite values.
 -}

module InfInt where

-- | Integer enriched with -infinity and +infinity
--
-- >>> (MInf, Only 4, PInf)
-- (-oo,4,+oo)
-- >>> MInf < Only 4
-- True
-- >>> PInf > Only 4
-- True
data InfInt
    -- | Minus infinity
    = MInf
    -- | An integer
    | Only Integer
    -- | Plus infinity
    | PInf
    deriving (Eq, Ord)

-- | Shows either a number of +infinity
--
-- >>> show MInf
-- "-oo"
-- >>> show PInf
-- "+oo"
-- >>> show (Only 8)
-- "8"
instance Show InfInt where
    show MInf = "-oo"
    show PInf = "+oo"
    show (Only a) = show a

-- | Usual arithmetic on integers with +oo and -oo
--
-- >>> PInf + PInf
-- +oo
-- >>> MInf + 4
-- -oo
-- >>> 4 * PInf
-- +oo
-- >>> 3 - PInf
-- -oo
-- >>> -PInf
-- -oo
-- >>> abs MInf
-- +oo
-- >>> signum PInf
-- 1
-- >>> signum MInf
-- -1
--
-- Some operations with +oo and -oo might yield undefined values.
--
-- >>> PInf + MInf
-- *** Exception: undefined
-- ...
-- >>> MInf + PInf
-- *** Exception: undefined
-- ...
-- >>> PInf - PInf
-- *** Exception: undefined
-- ...
-- >>> MInf - MInf
-- *** Exception: undefined
-- ...
-- >>> PInf * 0
-- *** Exception: undefined
-- ...
-- >>> 0 * PInf
-- *** Exception: undefined
-- ...
-- >>> MInf * 0
-- *** Exception: undefined
-- ...
-- >>> 0 * MInf
-- *** Exception: undefined
-- ...
instance Num InfInt where
    (+) PInf PInf        = PInf
    (+) PInf MInf     = error "undefined"
    (+) MInf PInf      = error "undefined"
    (+) PInf a        = PInf 
    (+) a   PInf     = PInf 
    (+) MInf a        = MInf 
    (+) a   MInf     = MInf 
    negate PInf     =  MInf
    negate MInf     =  PInf
    (*) 0 PInf   = error "undefined"
    (*) PInf 0   = error "undefined"
    (*) 0 MInf   = error "undefined"
    (*) MInf 0   = error "undefined"
    (*) a PInf   = PInf
    signum 0    =  0
    signum PInf = 1
    signum MInf = fromInteger (0-1)
    abs  MInf       = PInf
    abs  PInf       = PInf  
    fromInteger a = Only a
