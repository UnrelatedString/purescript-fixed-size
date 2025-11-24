-- | TODO: module documentation

module Data.Sized
  ( class Sized
  , Peano
  , class Induct
  ) where

import Prelude
import Data.NonEmpty (NonEmpty(..))

-- | The kind of inductively defined non-negative integers at the type level.
data Peano

foreign import data Zero :: Peano

foreign import data Succ :: Peano -> Peano

-- | As `Peano` can't be strictly closed,
-- | this class treats all types other than `Succ`
-- | as equivalent to `Zero`.

class Induct :: forall k. Peano -> (k -> k) -> k -> k -> Constraint
class Induct n succ zero result | n succ zero -> result

instance Induct n succ zero prev => Induct (Succ n) succ zero (succ prev)
else instance Induct closedZero succ zero zero

class Sized a
