-- | TODO: module documentation

module Data.Sized
  ( class Sized
  , Index
  , Peano
  , class Induct
  , class InfallibleKey
  , itemAt
  ) where

import Prelude
import Data.NonEmpty (NonEmpty(..))
import Prim.Int (class Add)
import Prim.TypeError as TypeError

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

class IntPeano :: Int -> Peano -> Constraint
class IntPeano int peano | int -> peano, peano -> int

instance (IntPeano prevInt prevPeano, Add prevInt 1 int) => IntPeano int (Succ peano)
else instance IntPeano 0 Zero
else instance TypeError.Fail
  ( TypeError.Text "Tried to convert negative Int or nonstandard Peano"
  ) => IntPeano int peano

-- | `Sized` for the more general case of non-integer keys.
class InfallibleKey :: (Type -> Type) -> Type -> Constraint
class InfallibleKey f key where
  itemAt :: forall a. f a -> key -> a

-- | An index into a `Sized` container.
data Index :: Peano -> Type
data Index n = Index (forall f a. Sized f n => f a -> a) -- is this really the best way aaa

class (InfallibleKey f (Index n)) <= Sized f n
