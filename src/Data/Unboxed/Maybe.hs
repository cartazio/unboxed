{-# LANGUAGE MagicHash, UnboxedSums, NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE GADTs ,ExplicitNamespaces#-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE PatternSynonyms#-}
{-# LANGUAGE BangPatterns #-}

module Data.Unboxed.Maybe where

import GHC.Exts
import GHC.Types
--import qualified  Prelude as Pre
--import Data.Void

{-
theres some surprising desugaring  in pattern synonyms
--pattern MyJust :: a -> Maybe a
--pattern MyJust !a
data MyMaybe a = JustC a | NothingC

pattern MyJust :: a -> MyMaybe a
-- pattern MyJust !a = JustC a -- this fails
pattern MyJust a = JustC !a -- this is fine

-}

data family Maybe (x :: TYPE (r :: RuntimeRep))

data instance Maybe (a :: * ) where
  MaybeSum :: !(# a  | (# #) #) -> Maybe a

pattern Just :: ( a :: *) -> Maybe a
pattern Just a =  MaybeSum !(# a | #)

pattern Nothing :: forall (a :: *) .  Maybe a
pattern Nothing = MaybeSum !(# | (# #) #)

type TypeUnlifted = TYPE 'UnliftedRep

data instance Maybe (x :: TYPE 'UnliftedRep) where
  MaybeSumU :: !(# x | (# #) #) -> Maybe (x :: TYPE 'UnliftedRep)

pattern JustU :: forall (x :: TypeUnlifted) . x -> Maybe x
pattern JustU  a = MaybeSumU !(#  a |  #)
