-- | TODO: module documentation

module Data.Sized
  ( class Sized
  , Index
  , Peano
  , class Induct
  , class InfallibleKey
  , itemAt
  , RowKey(..)
  ) where

import Prelude
import Data.NonEmpty (NonEmpty(..))
import Prim.Int (class Add)
import Prim.Row (class Union)
import Prim.TypeError as TypeError
import Type.Proxy (Proxy(..))
import Data.Reflectable (reifyType)

import Data.Homogeneous.Record (Homogeneous, fromHomogeneous)
import Data.Homogeneous.Variant as Variant
import Data.Variant as Variant

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

-- | `Sized` for the more general case of non-sequential keys.
class InfallibleKey :: (Type -> Type) -> Type -> Constraint
class InfallibleKey f key where
  itemAt :: forall a. f a -> key -> a

newtype RowKey :: Row Type -> Type
newtype RowKey r = RowKey (Variant.Homogeneous r Unit)

instance Union r' unindexed r => InfallibleKey (Homogeneous r) (RowKey r') where
  itemAt h (RowKey key) = Variant.match (fromHomogeneous $ map const h) $ Variant.fromHomogeneous key

-- | An index into a `Sized` container.
data Index :: Peano -> Type
data Index n = Index (forall f a. Sized n f => f a -> a) -- is this really the best way aaa

-- | A `Sized n` container is one which has *at least* `n` elements indexable.
class (InfallibleKey f (Index n)) <= Sized n f
