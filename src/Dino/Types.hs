{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Dino.Types
  ( module Dino.Types
  , Inspectable
  ) where

import Dino.Prelude

import Data.Type.Equality ((:~:) (..), TestEquality (..))
import Data.Typeable (cast)
import Type.Reflection (typeRep)

import Dino.AST (Inspectable)

-- | Built-in Dino types
--
-- Whether or not a type is built-in is mostly an implementation detail.
type family BuiltIn a :: Bool where
  BuiltIn [a]    = 'True
  BuiltIn (a, b) = 'True
  BuiltIn a      = 'False

data DinoTypeRep a where
  ListType  :: DinoTypeRep a -> DinoTypeRep [a]
  PairType  :: DinoTypeRep a -> DinoTypeRep b -> DinoTypeRep (a, b)
  OtherType :: (BuiltIn a ~ 'False, DinoType a) => DinoTypeRep a
  -- The `BuiltIn` constraint ensures that `DinoTypeRep` is a canonical
  -- representation for built-in types. This allows us to give total definitions
  -- of functions like `listElemType`.

  -- The reason for having a separate constructor for list types is solely to be
  -- able to implement functions like `listElemType`.

  -- The constraints on `OtherType` are somewhat arbitrary. We may bring in
  -- other constraints in the future if that is needed by a particular back end.
  -- However, we should avoid using type classes from a back end directly.
  -- Rather use generic ones, such as `Data`. We can also have different
  -- constructors for different types; e.g. numeric types, enumerations, etc.

withType :: DinoTypeRep a -> (DinoType a => b) -> b
withType (ListType t)   b = withType t b
withType (PairType t u) b = withType t $ withType u b
withType OtherType      b = b

listTypeElem :: DinoTypeRep [a] -> DinoTypeRep a
listTypeElem (ListType t) = t
  -- This function is total due to the `BuiltIn` constraint on `OtherType`.

-- | This instance is complete in the sense that if @t ~ u@, then
-- @`testEquality` (trep :: `DinoTypeRep` t) (urep :: `DinoTypeRep` u)@ returns
-- @`Just` `Refl`@.
--
-- For example, @`BoolType`@ and @`EnumType` :: `DinoTypeRep` `Bool`@ are
-- considered equal.
instance TestEquality DinoTypeRep where
  testEquality :: forall t u. DinoTypeRep t -> DinoTypeRep u -> Maybe (t :~: u)
  testEquality t u = withType t $ withType u $
    testEquality (typeRep @t) (typeRep @u)
    -- Note: Because `DinoTypeRep` is a canonical representation of Dino types,
    -- testing equality via `typeRep` should correspond to structural equality.

class (Eq a, Show a, Typeable a, Inspectable a) => DinoType a where
  dinoTypeRep :: DinoTypeRep a

  default dinoTypeRep :: (BuiltIn a ~ 'False) => DinoTypeRep a
  dinoTypeRep = OtherType

instance DinoType ()
instance DinoType Bool
instance DinoType Rational
instance DinoType Int
instance DinoType Integer
instance DinoType Float
instance DinoType Double
instance DinoType Text
instance DinoType a => DinoType (Maybe a)

instance DinoType a => DinoType [a] where
  dinoTypeRep = ListType dinoTypeRep

instance (DinoType a, DinoType b) => DinoType (a, b) where
  dinoTypeRep = PairType dinoTypeRep dinoTypeRep

-- | Dynamic type based on 'DinoType'
data Dinamic where
  Dinamic :: DinoType a => a -> Dinamic

fromDinamic :: DinoType a => Dinamic -> Maybe a
fromDinamic (Dinamic a) = cast a
