{-# LANGUAGE MagicHash, UnboxedSums, NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE GADTs ,ExplicitNamespaces#-}
{-# LANGUAGE UnboxedTuples #-}

module Data.Unboxed.Maybe where

import GHC.Exts
import GHC.Types
--import Prelude (undefined)
--import Data.Void

data family Maybe(x :: TYPE (r :: RuntimeRep))

data instance Maybe (a :: * ) where
  MaybeSum :: (# a  | (# #) #) -> Maybe a
data instance Maybe (x :: TYPE 'UnliftedRep) where
  MaybeSumU :: (# x | (# #) #) -> Maybe (x :: TYPE 'UnliftedRep)
