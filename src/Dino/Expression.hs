{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}

-- | General tagless expressions

module Dino.Expression where

import Dino.Prelude
import qualified Prelude

import Control.Applicative (liftA, liftA2)
import Control.Error (headMay)
import Control.Monad ((>=>), ap, foldM)
import Control.Monad.Loops (dropWhileM, firstM)
import Data.Bifunctor (Bifunctor (..))
import Data.List ((\\))
import Data.String (IsString (..))
import Data.Text (Text)
import GHC.TypeLits (KnownSymbol, Symbol)
import qualified GHC.Records as GHC
import GHC.Stack

import Dino.Types



--------------------------------------------------------------------------------
-- * Expression classes and constructs
--------------------------------------------------------------------------------

----------------------------------------
-- ** Constants
----------------------------------------

-- | Constant expressions
--
-- The default implementation is for 'Applicative' interpretations.
class ConstExp e where
  -- | Make a Dino literal from a Haskell value
  lit :: DinoType a => a -> e a

  default lit :: Applicative e => a -> e a
  lit = pure

true, false :: ConstExp e => e Bool
true = lit True
false = lit False

-- | Constant text expression
--
-- With @OverloadedStrings@ enabled, text literals can be written simply as
-- @"..."@.
text :: ConstExp e => Text -> e Text
text = lit



----------------------------------------
-- ** Numeric expressions
----------------------------------------

-- | Numeric expressions
--
-- The default implementations are for 'Applicative' interpretations.
class NumExp e where
  add   :: Num a => e a -> e a -> e a
  sub   :: Num a => e a -> e a -> e a
  mul   :: Num a => e a -> e a -> e a
  absE  :: Num a => e a -> e a
  signE :: Num a => e a -> e a

  -- | Convert an integer to any numeric type
  fromIntegral :: (Integral a, DinoType b, Num b) => e a -> e b

  -- | @`floor` x@ returns the greatest integer not greater than @x@
  floor :: (RealFrac a, DinoType b, Integral b) => e a -> e b

  -- | @`truncate` x@ returns the integer nearest @x@ between zero and @x@
  truncate :: (RealFrac a, DinoType b, Integral b) => e a -> e b

  -- | Round to the specified number of decimals
  roundN :: RealFrac a => Int -> e a -> e a
    -- TODO This function doesn't make much sense for non-decimal
    -- representations. Use a decimal representation.

  default add          :: (Applicative e, Num a) => e a -> e a -> e a
  default sub          :: (Applicative e, Num a) => e a -> e a -> e a
  default mul          :: (Applicative e, Num a) => e a -> e a -> e a
  default absE         :: (Applicative e, Num a) => e a -> e a
  default signE        :: (Applicative e, Num a) => e a -> e a
  default fromIntegral :: (Applicative e, Integral a, Num b) => e a -> e b
  default floor        :: (Applicative e, RealFrac a, Integral b) => e a -> e b
  default truncate     :: (Applicative e, RealFrac a, Integral b) => e a -> e b
  default roundN       :: (Applicative e, RealFrac a) => Int -> e a -> e a

  add          = liftA2 (+)
  sub          = liftA2 (-)
  mul          = liftA2 (*)
  absE         = liftA abs
  signE        = liftA signum
  fromIntegral = liftA Prelude.fromIntegral
  floor        = liftA (Prelude.fromInteger . Prelude.floor)
  truncate     = liftA (Prelude.fromInteger . Prelude.truncate)
  roundN n     = liftA roundN'
    where
      roundN' a = (fromInteger $ Prelude.round $ a * (10^n)) / (10.0^^n)
        -- https://stackoverflow.com/questions/12450501/round-number-to-specified-number-of-digits#12450771

-- | Convert an 'Integer' to any numeric type
fromInt :: (NumExp e, DinoType a, Num a) => e Integer -> e a
fromInt = fromIntegral
  -- We cannot override the name `fromInteger`, since that's used for desugaring
  -- numeric literals.

-- | Fractional expressions
--
-- The default implementation is for 'Applicative' interpretations.
class FracExp e where
  -- | Division
  fdiv :: (Fractional a, Eq a) => e a -> e a -> e a
    -- `Eq` is useful for catching division by zero.

  default fdiv :: (Applicative e, Fractional a) => e a -> e a -> e a
  fdiv = liftA2 (/)

-- | Division that returns 0 when the denominator is 0
(./) ::
     ( ConstExp e
     , FracExp e
     , CompareExp e
     , CondExpFO e
     , DinoType a
     , Fractional a
     )
  => e a
  -> e a
  -> e a
a ./ b = ifThenElse (b == lit 0) (lit 0) (fdiv a b)



----------------------------------------
-- ** Logic expressions
----------------------------------------

-- | Logic expressions
--
-- The default implementations are for 'Applicative' interpretations.
class LogicExp e where
  not  :: e Bool -> e Bool
  conj :: e Bool -> e Bool -> e Bool
  disj :: e Bool -> e Bool -> e Bool
  xor  :: e Bool -> e Bool -> e Bool

  default not  :: Applicative e => e Bool -> e Bool
  default conj :: Applicative e => e Bool -> e Bool -> e Bool
  default disj :: Applicative e => e Bool -> e Bool -> e Bool
  default xor  :: Applicative e => e Bool -> e Bool -> e Bool

  not  = liftA Prelude.not
  conj = liftA2 (Prelude.&&)
  disj = liftA2 (Prelude.||)
  xor  = liftA2 (Prelude./=)

(&&), (||) :: LogicExp e => e Bool -> e Bool -> e Bool

(&&) = conj
(||) = disj

infixr 3 &&
infixr 2 ||



----------------------------------------
-- ** Comparisons
----------------------------------------

-- | Comparisons
--
-- The default implementations are for 'Applicative' interpretations.
class CompareExp e where
  eq  :: Eq a  => e a -> e a -> e Bool
  neq :: Eq a  => e a -> e a -> e Bool
  lt  :: Ord a => e a -> e a -> e Bool
  gt  :: Ord a => e a -> e a -> e Bool
  lte :: Ord a => e a -> e a -> e Bool
  gte :: Ord a => e a -> e a -> e Bool
  min :: Ord a => e a -> e a -> e a
  max :: Ord a => e a -> e a -> e a

  default eq  :: (Applicative e, Eq a)  => e a -> e a -> e Bool
  default neq :: (Applicative e, Eq a)  => e a -> e a -> e Bool
  default lt  :: (Applicative e, Ord a) => e a -> e a -> e Bool
  default gt  :: (Applicative e, Ord a) => e a -> e a -> e Bool
  default lte :: (Applicative e, Ord a) => e a -> e a -> e Bool
  default gte :: (Applicative e, Ord a) => e a -> e a -> e Bool
  default min :: (Applicative e, Ord a) => e a -> e a -> e a
  default max :: (Applicative e, Ord a) => e a -> e a -> e a

  eq  = liftA2 (Prelude.==)
  neq = liftA2 (Prelude./=)
  lt  = liftA2 (Prelude.<)
  gt  = liftA2 (Prelude.>)
  lte = liftA2 (Prelude.<=)
  gte = liftA2 (Prelude.>=)
  min = liftA2 Prelude.min
  max = liftA2 Prelude.max

(==), (/=) :: (Eq a, CompareExp e) => e a -> e a -> e Bool
(==) = eq
(/=) = neq

(<), (>), (<=), (>=) :: (Ord a, CompareExp e) => e a -> e a -> e Bool
(<)  = lt
(>)  = gt
(<=) = lte
(>=) = gte

infix 4 ==, /=, <, >, <=, >=

-- | Check equality against a constant value
(==!) :: (ConstExp e, CompareExp e, DinoType a) => e a -> a -> e Bool
a ==! b = a == lit b

infix 4 ==!



----------------------------------------
-- ** Conditionals
----------------------------------------

-- | Representation of a case in 'cases'
data a :-> b = a :-> b
  deriving (Eq, Show, Foldable, Functor, Traversable)

instance Bifunctor (:->) where
  bimap f g (a :-> b) = f a :-> g b

-- | Construct a case in 'cases', 'match', etc.
--
-- Example:
--
-- @
-- beaufortScale :: _ => `Exp` e a -> `Exp` e `Text`
-- beaufortScale v = `match` v
--   [ (`<` 0.5)   `-->` "calm"
--   , (`<` 13.8)  `-->` "breeze"
--   , (`<` 24.5)  `-->` "gale" ]
--   ( `Otherwise` `-->` "storm" )
-- @
(-->) :: a -> b -> (a :-> b)
(-->) = (:->)

infix 1 :->, -->

-- | Marker for the default case in 'cases'
data Otherwise = Otherwise

-- | Helper class to 'CondExp' containing only first-order constructs
--
-- The reason for having this class is that there are types for which
-- 'CondExpFO' can be derived but 'CondExp' cannot.
class CondExpFO e where
  -- | Construct an optional value that is present
  just :: e a -> e (Maybe a)

  -- | Case expression
  cases ::
       [e Bool :-> e a] -- ^ Guarded expressions
    -> (Otherwise :-> e a) -- ^ Fall-through case
    -> e a

  -- | Case expression without fall-through
  --
  -- Evaluation may fail if the cases are not complete.
  partial_cases ::
       HasCallStack
    => [e Bool :-> e a] -- ^ Guarded expressions
    -> e a

  default just :: Applicative e => e a -> e (Maybe a)
  just = liftA Just

  default cases :: Monad e => [e Bool :-> e a] -> (Otherwise :-> e a) -> e a
  cases cs (_ :-> d) = do
    f <- firstM (\(c :-> _) -> c) cs
    case f of
      Nothing -> d
      Just (_ :-> a) -> a

  default partial_cases :: (Monad e, HasCallStack) => [e Bool :-> e a] -> e a
  partial_cases = default_partial_cases

-- | Expressions supporting conditionals
--
-- The default implementations are for monadic interpretations.
class CondExpFO e => CondExp e where
  -- | Deconstruct an optional value
  maybe ::
       DinoType a
    => e b -- ^ Result when 'nothing'
    -> (e a -> e b) -- ^ Result when 'just'
    -> e (Maybe a) -- ^ Value to deconstruct
    -> e b

  default maybe :: Monad e => e b -> (e a -> e b) -> e (Maybe a) -> e b
  maybe n j m = Prelude.maybe n (j . return) =<< m

default_partial_cases :: (CondExpFO e, HasCallStack) => [e Bool :-> e a] -> e a
default_partial_cases cs =
  cases cs $ (Otherwise --> error "partial_cases: no matching case")

-- | Construct an optional value that is missing
nothing :: (ConstExp e, DinoType a) => e (Maybe a)
nothing = lit Nothing

isJust :: (ConstExp e, CondExp e, DinoType a) => e (Maybe a) -> e Bool
isJust = maybe false (const true)

-- | Case expression using Boolean functions for matching
match ::
     CondExpFO e
  => a -- ^ Scrutinee
  -> [(a -> e Bool) :-> e b] -- ^ Cases
  -> (Otherwise :-> e b) -- ^ Fall-through case
  -> e b
match a = cases . map (first ($ a))

-- | Case expression matching a value against constants
--
-- Example:
--
-- @
-- operate c a = `matchConst` c
--   ['+' `-->` a + 1
--   ,'-' `-->` a - 1
--   ]
--   (`Otherwise` `-->` a)
-- @
matchConst ::
     (ConstExp e, CompareExp e, CondExpFO e, DinoType a)
  => e a -- ^ Scrutinee
  -> [a :-> e b] -- ^ Cases
  -> (Otherwise :-> e b) -- ^ Fall-through case
  -> e b
matchConst a = match a . map (first ((==) . lit))

-- | A Version of 'matchConst' for enumerations where the cases cover the whole
-- domain
--
-- An error is thrown if the cases do not cover the whole domain.
matchConstFull ::
     ( ConstExp e
     , CompareExp e
     , CondExpFO e
     , DinoType a
     , Show a
     , Enum a
     , Bounded a
     , HasCallStack
     )
  => e a -- ^ Scrutinee
  -> [a :-> e b] -- ^ Cases
  -> e b
matchConstFull a cs
  | null missing = partial_cases $ map (first (a ==!)) cs
  | otherwise = error $ "matchConstFull: missing cases " ++ show missing
  where
    domain = [minBound .. maxBound]
    missing = domain \\ [b | b :-> _ <- cs]

-- | Conditional expression
--
-- Enable @RebindableSyntax@ to use the standard syntax @if a then b else c@
-- for calling this function.
ifThenElse ::
     CondExpFO e
  => e Bool -- ^ Condition
  -> e a -- ^ True branch
  -> e a -- ^ False branch
  -> e a
ifThenElse c t f = cases [c --> t] (Otherwise --> f)

fromMaybe :: (CondExp e, DinoType a) => e a -> e (Maybe a) -> e a
fromMaybe n = maybe n id



----------------------------------------
-- ** Lists
----------------------------------------

-- | Helper class to 'ListExp' containing only first-order constructs
--
-- The reason for having this class is that there are types for which
-- 'ListExpFO' can be derived but 'ListExp' cannot.
class ListExpFO e where
  range ::
       Enum a
    => e a -- ^ Lower bound (inclusive)
    -> e a -- ^ Upper bound (inclusive)
    -> e [a]

  list   :: DinoType a => [e a] -> e [a]
  headE  :: e [a] -> e (Maybe a)
  append :: e [a] -> e [a] -> e [a]

  default range  :: (Applicative e, Enum a) => e a -> e a -> e [a]
  default list   :: Applicative e => [e a] -> e [a]
  default headE  :: Applicative e => e [a] -> e (Maybe a)
  default append :: Applicative e => e [a] -> e [a] -> e [a]

  range  = liftA2 $ \l u -> [l .. u]
  list   = sequenceA
  headE  = liftA headMay
  append = liftA2 (++)

class ListExpFO e => ListExp e where
  mapE       :: DinoType a => (e a -> e b) -> e [a] -> e [b]
  dropWhileE :: DinoType a => (e a -> e Bool) -> e [a] -> e [a]

  -- | Left fold
  foldE ::
       (DinoType a, DinoType b)
    => (e a -> e b -> e a) -- ^ Reducer function
    -> e a -- ^ Initial value
    -> e [b] -- ^ List to reduce (traversed left-to-right)
    -> e a

  default mapE       :: Monad e => (e a -> e b) -> e [a] -> e [b]
  default dropWhileE :: Monad e => (e a -> e Bool) -> e [a] -> e [a]
  default foldE      :: Monad e => (e a -> e b -> e a) -> e a -> e [b] -> e a

  mapE f as       = mapM (f . return) =<< as
  dropWhileE p as = dropWhileM (p . return) =<< as

  foldE f a bs = do
    a' <- a
    bs' <- bs
    foldM (\aa bb -> f (return aa) (return bb)) a' bs'



----------------------------------------
-- ** Tuples
----------------------------------------

class TupleExp e where
  pair :: e a -> e b -> e (a, b)
  fstE :: e (a, b) -> e a
  sndE :: e (a, b) -> e b

  default pair :: Applicative e => e a -> e b -> e (a, b)
  default fstE :: Applicative e => e (a, b) -> e a
  default sndE :: Applicative e => e (a, b) -> e b

  pair = liftA2 (,)
  fstE = liftA fst
  sndE = liftA snd



----------------------------------------
-- ** Let bindings
----------------------------------------

class LetExp e where
  -- | Share a value in a calculation
  --
  -- The default implementation of 'letE' implements call-by-value.
  letE ::
       DinoType a
    => Text         -- ^ Variable base name
    -> e a          -- ^ Value to share
    -> (e a -> e b) -- ^ Body
    -> e b

  default letE :: Monad e => Text -> e a -> (e a -> e b) -> e b
  letE _ a body = a >>= body . return

-- | Share a value in a calculation
--
-- Like 'letE' but with the variable base name fixed to \"share\".
share ::
     (LetExp e, DinoType a)
  => e a          -- ^ Value to share
  -> (e a -> e b) -- ^ Body
  -> e b
share = letE "share"

-- | Make a function with a shared argument
--
-- @
-- `shared` = `flip` `share`
-- @
--
-- Like 'letE' but with the variable base name fixed to \"share\".
shared ::
     (LetExp e, DinoType a)
  => (e a -> e b) -- ^ Body
  -> e a          -- ^ Value to share
  -> e b
shared = flip share



----------------------------------------
-- ** Records
----------------------------------------

data Field (f :: Symbol) = Field

class FieldExp e where
  getField ::
       (KnownSymbol f, HasField f r a, DinoType a) => proxy f -> e r -> e a

  default getField ::
       forall proxy f r a. (Applicative e, KnownSymbol f, HasField f r a)
    => proxy f
    -> e r
    -> e a
  getField _ = liftA (GHC.getField @f)

instance (f1 ~ f2) => IsLabel f1 (Field f2) where
  fromLabel = Field

-- | Extract a field from a record
--
-- Use as follows (with @OverloadedLabels@):
--
-- > field #name $ field #driver car
field ::
     (FieldExp e, KnownSymbol f, HasField f r a, DinoType a)
  => Field f
  -> e r
  -> e a
field = getField

-- | Extract a field from a record
--
-- Use as follows (with @OverloadedLabels@):
--
-- > #name <. #driver <. car
(<.) ::
     (FieldExp e, KnownSymbol f, HasField f r a, DinoType a)
  => Field f
  -> e r
  -> e a
(<.) = getField

infixr 9 <.



----------------------------------------
-- ** Annotations
----------------------------------------

class AnnExp ann e where
  -- | Annotate an expression
  ann :: ann -> e a -> e a
  ann _ = id



----------------------------------------
-- ** Assertions
----------------------------------------

class AssertExp e where
  -- | Assert that a condition is true
  --
  -- Interpretations can choose whether to ignore the assertion or to check its
  -- validity. The default implementation ignores the assertion.
  --
  -- The following must hold for any monadic interpretation:
  --
  -- @
  -- `assert` lab c a
  --   `==`
  -- (`assert` lab c (`return` ()) `>>` `return` a)
  -- @
  assert ::
       Text -- ^ Assertion label
    -> e Bool -- ^ Condition that should be true
    -> e a -- ^ Expression to attach the assertion to
    -> e a
  assert _ _ = id

  -- | Assert that an expression is semantically equivalent to a reference
  -- expression
  --
  -- Interpretations can choose whether to ignore the assertion or to check its
  -- validity. The default implementation ignores the assertion.
  --
  -- The following must hold for any monadic interpretation:
  --
  -- @
  -- `assertEq` lab ref act
  --   `==`
  -- ( do a <- act
  --      `assertEq` lab ref (`return` a)
  --      return a
  -- )
  -- @
  assertEq ::
       (Eq a, Show a) -- TODO Use `Pretty`?
    => Text -- ^ Assertion label
    -> e a -- ^ Reference expression
    -> e a -- ^ Actual expression
    -> e a
  assertEq _ _ act = act
    -- Having a separate function for equality avoids the problem of "Boolean
    -- blindness". For example, a diff of the two expressions can be shown when
    -- they are not equal.



----------------------------------------
-- ** Concrete expression wrapper
----------------------------------------

-- | Useful wrapper to get a concrete type for tagless DSL expressions
--
-- The problem solved by this type can be explained as follows:
--
-- Suppose you write a numeric expression with the most general type:
--
-- > myExp1 :: Num e => e
-- > myExp1 = 1+2
--
-- And suppose you define an evaluation function as follows:
--
--
-- > eval1 :: (forall e . (ConstExp e, NumExp e) => e a) -> a
-- > eval1 = runIdentity
--
-- The problem is that we cannot pass @myExp1@ to @eval1@:
--
-- > test1 :: Int
-- > test1 = eval1 myExp1
--
-- This leads to:
--
-- > • Could not deduce (Num (e Int)) ...
--
-- And we don't want to change @eval1@ to
--
-- > eval1 :: (forall e . (ConstExp e, NumExp e, Num (e a)) => e a) -> a
--
-- since this requires the expression to return a number (and not e.g. a
-- Boolean), and it also doesn't help to satisfy any internal numeric
-- expressions that may use a different type than @a@.
--
-- Instead, the solution is to use 'Exp' as follows:
--
-- > myExp2 :: (ConstExp e, NumExp e, Num a) => Exp e a
-- > myExp2 = 1+2
-- >
-- > eval2 :: (forall e . (ConstExp e, NumExp e) => Exp e a) -> a
-- > eval2 = runIdentity . unExp
-- >
-- > test2 :: Int
-- > test2 = eval2 myExp2
--
-- The trick is that there exists an instance
--
-- > instance (Num a, ConstExp e, NumExp e) => Num (Exp e a)
--
-- So it is enough for @eval2@ to supply constraints on @e@, and it will
-- automatically imply the availability of the `Num` instance.
newtype Exp e a = Exp
  { unExp :: e a
  } deriving ( Eq
             , Show
             , Functor
             , Applicative
             , Monad
             , ConstExp
             , NumExp
             , FracExp
             , LogicExp
             , CompareExp
             , CondExpFO
             , CondExp
             , ListExpFO
             , ListExp
             , LetExp
             , FieldExp
             , AnnExp ann
             , AssertExp
             )

instance (ConstExp e, IsString a, DinoType a) => IsString (Exp e a) where
  fromString = lit . fromString

instance (ConstExp e, NumExp e, DinoType a, Num a) => Num (Exp e a) where
  fromInteger = Exp . lit . fromInteger
  (+) = add
  (-) = sub
  (*) = mul
  abs = absE
  signum = signE

instance (ConstExp e, NumExp e, FracExp e, DinoType a, Fractional a) =>
         Fractional (Exp e a) where
  fromRational = Exp . lit . fromRational
  (/) = fdiv

instance (FieldExp e1, e1 ~ e2, KnownSymbol f, HasField f r a, DinoType a) =>
         IsLabel f (Exp e1 r -> Exp e2 a) where
  fromLabel = getField (Field @f)



--------------------------------------------------------------------------------
-- * Derived operations
--------------------------------------------------------------------------------

----------------------------------------
-- ** Operations on Dino lists
----------------------------------------

sumE :: (ConstExp e, NumExp e, ListExp e, DinoType a, Num a) => e [a] -> e a
sumE = foldE add (lit 0)

andE :: (ConstExp e, LogicExp e, ListExp e) => e [Bool] -> e Bool
andE = foldE (&&) true

orE :: (ConstExp e, LogicExp e, ListExp e) => e [Bool] -> e Bool
orE = foldE (||) false

allE ::
     (ConstExp e, LogicExp e, ListExp e, DinoType a)
  => (e a -> e Bool)
  -> e [a]
  -> e Bool
allE p = andE . mapE p

anyE ::
     (ConstExp e, LogicExp e, ListExp e, DinoType a)
  => (e a -> e Bool)
  -> e [a]
  -> e Bool
anyE p = orE . mapE p

find ::
     (LogicExp e, ListExp e, DinoType a)
  => (e a -> e Bool)
  -> e [a]
  -> e (Maybe a)
find p = headE . dropWhileE (not . p)

(<++>) :: ListExpFO e => e [a] -> e [a] -> e [a]
(<++>) = append



----------------------------------------
-- ** Operations on Haskell lists
----------------------------------------

and :: (ConstExp e, LogicExp e) => [e Bool] -> e Bool
and = foldr (&&) true

or :: (ConstExp e, LogicExp e) => [e Bool] -> e Bool
or = foldr (||) false

all :: (ConstExp e, LogicExp e) => (a -> e Bool) -> [a] -> e Bool
all p = and . map p

any :: (ConstExp e, LogicExp e) => (a -> e Bool) -> [a] -> e Bool
any p = or . map p



----------------------------------------
-- ** Optional monad
----------------------------------------

-- | 'Optional' expressions with a 'Monad' instance
--
-- 'Optional' is handy to avoid nested uses of 'maybe'. As an example, here is a
-- safe division function:
--
-- > safeDiv :: _ => e a -> e a -> Optional e (e a)
-- > safeDiv a b = suppose $
-- >   if (b /= lit 0)
-- >     then just (fdiv a b)
-- >     else nothing
--
-- And here is a calculation that defaults to 0 if any of the divisions fails:
--
-- > foo :: _ => Exp e Double -> Exp e Double -> Exp e Double
-- > foo a b = fromOptional 0 $ do
-- >   x <- safeDiv a b
-- >   y <- safeDiv b x
-- >   safeDiv x y
data Optional e a where
  Return :: a -> Optional e a
  Bind :: DinoType a => e (Maybe a) -> (e a -> Optional e b) -> Optional e b
  -- Inspired by the Operational monad

instance Functor (Optional e) where
  fmap f (Return a) = Return $ f a
  fmap f (Bind m k) = Bind m (fmap f . k)

instance Applicative (Optional e) where
  pure = Return
  (<*>) = ap

instance Monad (Optional e) where
  Return a >>= k = k a
  Bind m k >>= l = Bind m (k >=> l)

-- | Lift an optional expression to 'Optional'
suppose :: DinoType a => e (Maybe a) -> Optional e (e a)
suppose a = Bind a Return

-- | Convert from 'Optional' value to an optional expression
optional ::
     (ConstExp e, CondExp e, LetExp e, DinoType a, DinoType b)
  => e b -- ^ Result if missing
  -> (e a -> e b) -- ^ Result if present
  -> Optional e (e a) -- ^ Value to examine
  -> e b
optional n j o = share n $ \n' ->
  let go (Return a) = j a
      go (Bind m k) = maybe n' (go . k) m
   in go o

runOptional ::
     (ConstExp e, CondExp e, LetExp e, DinoType a)
  => Optional e (e a)
  -> e (Maybe a)
runOptional = optional nothing just

-- | Extract an 'Optional' value
fromOptional ::
     (ConstExp e, CondExp e, LetExp e, DinoType a)
  => e a -- ^ Default value (in case the 'Optional' value is missing)
  -> Optional e (e a)
  -> e a
fromOptional d = optional d id
