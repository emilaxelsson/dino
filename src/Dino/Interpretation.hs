{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

-- | Interpretation of tagless expressions

module Dino.Interpretation where

import Dino.Prelude
import qualified Prelude

import Control.Exception (Exception, throw)
import Control.Monad (foldM, unless)
import Control.Monad.Catch (MonadThrow (..))
import Control.Monad.Except (MonadError (..))
import Control.Monad.Identity (Identity (..))
import Control.Monad.Loops (dropWhileM)
import Control.Monad.Reader (MonadReader (..), ReaderT (..))
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Writer (WriterT)
import Data.Coerce (coerce)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import Data.Proxy (Proxy (..))
import Data.Ratio (denominator, numerator)
import GHC.TypeLits (symbolVal)

import Dino.AST
import Dino.Types
import Dino.Expression



-- Some terminology used in this module:
--
-- We'll refer to the various `...Exp` classes as "syntax classes". (To be
-- exact, the class definitions provide syntax, while the instances provide
-- semantics.)
--
-- "First-order syntax" (FOS) refers to classes that only contain first-order
-- constructs (i.e. no negative occurrences of the `e` type).
--
-- "Higher-order syntax" (HOS) refers to classes that contain higher-order
-- constructs.
--
-- The "intensional" counterpart of a HOS class is a FOS class that uses
-- explicit named variables instead. Intensional classes are only used
-- internally to enable certain interpretations. They should not be exported to
-- the user.



--------------------------------------------------------------------------------
-- * Type checking
--------------------------------------------------------------------------------

-- The `DinoTypeRep` interpretation serves as a proof that Dino expressions can
-- only calculate with "supported types". Or, pragmatically, it means that we
-- can make arbitrary type information available to any sub-expression via
-- `DinoTypeRep`.

instance ConstExp DinoTypeRep where
  lit _ = dinoTypeRep

instance NumExp DinoTypeRep where
  add t _         = t
  sub t _         = t
  mul t _         = t
  absE t          = t
  signE t         = t
  fromIntegral _  = dinoTypeRep
  floor _         = dinoTypeRep
  truncate _      = dinoTypeRep
  roundN _ t      = t

instance FracExp DinoTypeRep where
  fdiv t _ = t

instance LogicExp DinoTypeRep where
  not _    = dinoTypeRep
  conj _ _ = dinoTypeRep
  disj _ _ = dinoTypeRep
  xor _ _  = dinoTypeRep

instance CompareExp DinoTypeRep where
  eq  _ _ = dinoTypeRep
  neq _ _ = dinoTypeRep
  lt  _ _ = dinoTypeRep
  gt  _ _ = dinoTypeRep
  lte _ _ = dinoTypeRep
  gte _ _ = dinoTypeRep
  min t _ = t
  max t _ = t

instance CondExpFO DinoTypeRep where
  just t            = withType t OtherType
  cases _ (_ :-> t) = t
  partial_cases     = default_partial_cases

instance CondExp DinoTypeRep where
  maybe t _ _ = t

instance ListExpFO DinoTypeRep where
  range t _  = ListType t
  list _     = dinoTypeRep
  headE      = (\t -> withType t OtherType) . listTypeElem
  append t _ = t

instance ListExp DinoTypeRep where
  mapE f       = ListType . f . listTypeElem
  dropWhileE _ = id
  foldE _ t _  = t
  -- Note: `mapE` has to build the body before returning. This leads to
  -- quadratic complexity for nested maps.

instance TupleExp DinoTypeRep where
  pair                = PairType
  fstE (PairType t _) = t
  sndE (PairType _ t) = t

instance LetExp DinoTypeRep where
  letE _ t body = body t
  -- Note: `letE` has to build the body before returning. This leads to
  -- quadratic complexity for nested maps.

instance FieldExp DinoTypeRep where
  getField _ _ = dinoTypeRep

instance AnnExp ann DinoTypeRep

instance AssertExp DinoTypeRep



--------------------------------------------------------------------------------
-- * Monadic interpretation
--------------------------------------------------------------------------------

instance ConstExp Identity
instance NumExp Identity
instance FracExp Identity
instance LogicExp Identity
instance CompareExp Identity
instance CondExpFO Identity
instance CondExp Identity
instance ListExpFO Identity
instance ListExp Identity
instance TupleExp Identity
instance LetExp Identity
instance FieldExp Identity
instance AnnExp ann Identity
-- | Ignoring assertion for efficiency
instance AssertExp Identity

instance ConstExp Maybe
instance NumExp Maybe
instance FracExp Maybe
instance LogicExp Maybe
instance CompareExp Maybe
instance CondExpFO Maybe
instance CondExp Maybe
instance ListExpFO Maybe
instance ListExp Maybe
instance TupleExp Maybe
instance LetExp Maybe
instance FieldExp Maybe
instance AnnExp ann Maybe
-- | Ignoring assertion for efficiency
instance AssertExp Maybe

instance ConstExp (Either e)
instance NumExp (Either e)
instance FracExp (Either e)
instance LogicExp (Either e)
instance CompareExp (Either e)
instance CondExpFO (Either e)
instance CondExp (Either e)
instance ListExpFO (Either e)
instance ListExp (Either e)
instance TupleExp (Either e)
instance LetExp (Either e)
instance FieldExp (Either e)
instance AnnExp ann (Either e)
-- | Ignoring assertion for efficiency
instance AssertExp (Either e)

instance Monad m => ConstExp (ExceptT e m)
instance Monad m => NumExp (ExceptT e m)
instance Monad m => FracExp (ExceptT e m)
instance Monad m => LogicExp (ExceptT e m)
instance Monad m => CompareExp (ExceptT e m)
instance Monad m => CondExpFO (ExceptT e m)
instance Monad m => CondExp (ExceptT e m)
instance Monad m => ListExpFO (ExceptT e m)
instance Monad m => ListExp (ExceptT e m)
instance Monad m => TupleExp (ExceptT e m)
instance Monad m => LetExp (ExceptT e m)
instance Monad m => FieldExp (ExceptT e m)
instance            AnnExp ann (ExceptT e m)
-- | Ignoring assertion for efficiency
instance            AssertExp (ExceptT e m)

instance Applicative m => ConstExp (ReaderT env m)
instance Applicative m => NumExp (ReaderT env m)
instance Applicative m => FracExp (ReaderT env m)
instance Applicative m => LogicExp (ReaderT env m)
instance Applicative m => CompareExp (ReaderT env m)
instance Monad m       => CondExpFO (ReaderT env m)
instance Monad m       => CondExp (ReaderT env m)
instance Monad m       => ListExpFO (ReaderT env m)
instance Monad m       => ListExp (ReaderT env m)
instance Monad m       => TupleExp (ReaderT env m)
instance Monad m       => LetExp (ReaderT env m)
instance Monad m       => FieldExp (ReaderT env m)
instance                  AnnExp ann (ReaderT env m)
-- | Ignoring assertion for efficiency
instance                  AssertExp (ReaderT env m)

instance (Monoid t, Applicative m) => ConstExp (WriterT t m)
instance (Monoid t, Applicative m) => NumExp (WriterT t m)
instance (Monoid t, Applicative m) => FracExp (WriterT t m)
instance (Monoid t, Applicative m) => LogicExp (WriterT t m)
instance (Monoid t, Applicative m) => CompareExp (WriterT t m)
instance (Monoid t, Monad m)       => CondExpFO (WriterT t m)
instance (Monoid t, Monad m)       => CondExp (WriterT t m)
instance (Monoid t, Monad m)       => ListExpFO (WriterT t m)
instance (Monoid t, Monad m)       => ListExp (WriterT t m)
instance (Monoid t, Monad m)       => TupleExp (WriterT t m)
instance (Monoid t, Monad m)       => LetExp (WriterT t m)
instance (Monoid t, Monad m)       => FieldExp (WriterT t m)
instance                              AnnExp ann (WriterT t m)
-- | Ignoring assertion for efficiency
instance                              AssertExp (WriterT t m)

-- | Pure evaluation
eval :: Exp Identity a -> a
eval = coerce

-- | Functorial evaluation
--
-- Can, for example, have the type
--
-- @`evalF` :: `Exp` `Maybe` a -> `Maybe` a@
evalF :: Exp f a -> f a
evalF = coerce



--------------------------------------------------------------------------------
-- * Folding
--------------------------------------------------------------------------------

-- | A folding interpretation
--
-- Instances of expression classes work by monoidal folding over @e@ (see
-- 'foldMonoid').
newtype Fold e a = Fold {fold :: e}
  deriving (Eq, Show, Functor, Semigroup, Monoid)

instance Monoid e => Applicative (Fold e) where
  pure = mempty
  f <*> a = Fold (fold f <> fold a)

-- | N-ary folding functions
class FoldN f e | f -> e where
  -- | @`foldN` e (*)@  returns an n-ary function of the following form:
  --
  -- > \(Fold a) (Fold b) ... (Fold x) -> Fold (e * a * b * ... x)
  --
  -- (here @*@ denotes any binary operator, and it's assumed to be right-
  -- associative.)
  foldN :: e -> (e -> e -> e) -> f

instance FoldN (Fold e a) e where
  foldN e0 _ = Fold e0

instance FoldN f e => FoldN (Fold e a -> f) e where
  foldN e0 conc = \(Fold e) -> foldN (conc e0 e) conc

-- | @'foldMonoid' returns an n-ary function of the following form:
--
-- > \(Fold a) (Fold b) ... (Fold x) -> Fold (mempty <> a <> b <> ... x)
--
-- where 'mempty' and '<>' are the methods of the 'Monoid' class.
foldMonoid :: (FoldN f e, Monoid e) => f
foldMonoid = foldN mempty mappend

instance Monoid e => ConstExp   (Fold e)
instance Monoid e => NumExp     (Fold e)
instance Monoid e => FracExp    (Fold e)
instance Monoid e => LogicExp   (Fold e)
instance Monoid e => CompareExp (Fold e)
instance Monoid e => ListExpFO  (Fold e)
instance Monoid e => TupleExp   (Fold e)
instance Monoid e => FieldExp   (Fold e)
instance             AnnExp ann (Fold e)

-- | Interprets all branches
instance Monoid e => CondExpFO (Fold e) where
  cases cs (Otherwise :-> d) =
    Fold $ mconcat $ concat [[fold c, fold a] | (c :-> a) <- cs] ++ [fold d]

  partial_cases cs =
    Fold $ mconcat $ concat [[fold c, fold a] | (c :-> a) <- cs]

-- | Interprets all branches
instance Monoid e => CondExp (Fold e) where
  maybe n j m = coerce m <> coerce n <> coerce (j mempty)

instance Monoid e => ListExp (Fold e) where
  mapE f as       = coerce as <> coerce (f mempty)
  dropWhileE p as = coerce as <> coerce (p mempty)
  foldE f a as    = coerce a <> coerce as <> coerce (f mempty mempty)

instance Monoid e => LetExp (Fold e) where
  letE _ a f = coerce a <> f mempty

-- | Ignoring assertion
instance AssertExp (Fold e)

instance Monoid e => VarExp (Fold e) where
  varE _ = mempty

instance Semigroup e => CondIntensional (Fold e) where
  maybeI _ n j m = coerce m <> coerce n <> coerce j

instance Semigroup e => ListIntensional (Fold e) where
  mapI _ b as       = coerce as <> coerce b
  dropWhileI _ b as = coerce as <> coerce b
  foldI _ _ b a as  = coerce a <> coerce as <> coerce b

instance Semigroup e => LetIntensional (Fold e) where
  letI _ a b = coerce a <> coerce b



--------------------------------------------------------------------------------
-- * Product of interpretations
--------------------------------------------------------------------------------

-- | Product of two interpretations
--
-- The product is used to run two interpretations in parallel. Note that there
-- are no instances for HOS classes. Instead, use @`Intensional` (e1 :×: e2)@ in
-- order to derive an interpretation of HOS classes for products.
data (e1 :×: e2) a = (:×:)
  { prodFst :: e1 a
  , prodSnd :: e2 a
  }

mkProd ::
     (lang e1, lang e2)
  => proxy lang
  -> (forall e. lang e => e a)
  -> (e1 :×: e2) a
mkProd _ e = e :×: e

liftProd ::
     (lang e1, lang e2)
  => proxy lang
  -> (forall e. lang e => e a -> e b)
  -> (e1 :×: e2) a
  -> (e1 :×: e2) b
liftProd _ f (a1 :×: a2) = f a1 :×: f a2

liftProd2 ::
     (lang e1, lang e2)
  => proxy lang
  -> (forall e. lang e => e a -> e b -> e c)
  -> (e1 :×: e2) a
  -> (e1 :×: e2) b
  -> (e1 :×: e2) c
liftProd2 _ f (a1 :×: a2) (b1 :×: b2) = f a1 b1 :×: f a2 b2

liftProd3 ::
     (lang e1, lang e2)
  => proxy lang
  -> (forall e. lang e => e a -> e b -> e c -> e d)
  -> (e1 :×: e2) a
  -> (e1 :×: e2) b
  -> (e1 :×: e2) c
  -> (e1 :×: e2) d
liftProd3 _ f (a1 :×: a2) (b1 :×: b2) (c1 :×: c2) = f a1 b1 c1 :×: f a2 b2 c2

instance (ConstExp e1, ConstExp e2) => ConstExp (e1 :×: e2) where
  lit a = mkProd (Proxy @ConstExp) (lit a)

instance (NumExp e1, NumExp e2) => NumExp (e1 :×: e2) where
  add          = liftProd2 (Proxy @NumExp) add
  sub          = liftProd2 (Proxy @NumExp) sub
  mul          = liftProd2 (Proxy @NumExp) mul
  absE         = liftProd  (Proxy @NumExp) absE
  signE        = liftProd  (Proxy @NumExp) signE
  fromIntegral = liftProd  (Proxy @NumExp) fromIntegral
  floor        = liftProd  (Proxy @NumExp) floor
  truncate     = liftProd  (Proxy @NumExp) truncate
  roundN n     = liftProd  (Proxy @NumExp) (roundN n)

instance (FracExp e1, FracExp e2) => FracExp (e1 :×: e2) where
  fdiv = liftProd2 (Proxy @FracExp) fdiv

instance (LogicExp e1, LogicExp e2) => LogicExp (e1 :×: e2) where
  not  = liftProd  (Proxy @LogicExp) not
  conj = liftProd2 (Proxy @LogicExp) conj
  disj = liftProd2 (Proxy @LogicExp) disj
  xor  = liftProd2 (Proxy @LogicExp) xor

instance (CompareExp e1, CompareExp e2) => CompareExp (e1 :×: e2) where
  eq  = liftProd2 (Proxy @CompareExp) eq
  neq = liftProd2 (Proxy @CompareExp) neq
  lt  = liftProd2 (Proxy @CompareExp) lt
  gt  = liftProd2 (Proxy @CompareExp) gt
  lte = liftProd2 (Proxy @CompareExp) lte
  gte = liftProd2 (Proxy @CompareExp) gte
  min = liftProd2 (Proxy @CompareExp) min
  max = liftProd2 (Proxy @CompareExp) max

instance (CondExpFO e1, CondExpFO e2) => CondExpFO (e1 :×: e2) where
  just = liftProd (Proxy @CondExpFO) just

  cases cs (Otherwise :-> (d1 :×: d2)) =
    cases cs1 (Otherwise :-> d1) :×: cases cs2 (Otherwise :-> d2)
    where
      (cs1, cs2) =
        unzip [(c1 :-> a1, c2 :-> a2) | ((c1 :×: c2) :-> (a1 :×: a2)) <- cs]

  partial_cases cs = partial_cases cs1 :×: partial_cases cs2
    where
      (cs1, cs2) =
        unzip [(c1 :-> a1, c2 :-> a2) | ((c1 :×: c2) :-> (a1 :×: a2)) <- cs]

instance (ListExpFO e1, ListExpFO e2) => ListExpFO (e1 :×: e2) where
  range  = liftProd2 (Proxy @ListExpFO) range
  headE  = liftProd  (Proxy @ListExpFO) headE
  append = liftProd2 (Proxy @ListExpFO) append

  list as = list as1 :×: list as2
    where
      (as1, as2) = unzip [(a1, a2) | (a1 :×: a2) <- as]

instance (TupleExp e1, TupleExp e2) => TupleExp (e1 :×: e2) where
  pair = liftProd2 (Proxy @TupleExp) pair
  fstE = liftProd  (Proxy @TupleExp) fstE
  sndE = liftProd  (Proxy @TupleExp) sndE

instance (FieldExp e1, FieldExp e2) => FieldExp (e1 :×: e2) where
  getField f = liftProd (Proxy @FieldExp) (getField f)

instance (AnnExp ann e1, AnnExp ann e2) => AnnExp ann (e1 :×: e2) where
  ann a = liftProd (Proxy @(AnnExp ann)) (ann a)

instance (AssertExp e1, AssertExp e2) => AssertExp (e1 :×: e2) where
  assert   lab = liftProd2 (Proxy @AssertExp) (assert lab)
  assertEq lab = liftProd2 (Proxy @AssertExp) (assertEq lab)

instance (VarExp e1, VarExp e2) => VarExp (e1 :×: e2) where
  varE v = mkProd (Proxy @VarExp) (varE v)

instance (CondIntensional e1, CondIntensional e2) =>
         CondIntensional (e1 :×: e2) where
  maybeI v = liftProd3 (Proxy @CondIntensional) (maybeI v)

instance (ListIntensional e1, ListIntensional e2) =>
         ListIntensional (e1 :×: e2) where
  mapI v       = liftProd2 (Proxy @ListIntensional) (mapI v)
  dropWhileI v = liftProd2 (Proxy @ListIntensional) (dropWhileI v)
  foldI va vb  = liftProd3 (Proxy @ListIntensional) (foldI va vb)

instance (LetIntensional e1, LetIntensional e2) =>
         LetIntensional (e1 :×: e2) where
  letI v = liftProd2 (Proxy @LetIntensional) (letI v)



--------------------------------------------------------------------------------
-- * Intensional interpretation
--------------------------------------------------------------------------------

-- Intensional interpretation essentially means to analyze the expression
-- syntactically rather than evaluating it.
--
-- <https://en.wikipedia.org/wiki/Extensional_and_intensional_definitions>



-- | Representation of the set of variables used by bindings in an expression.
-- An entry @(v, n)@ means that the base name @v@ is used possibly appended with
-- a number that is at most @n@.
--
-- Since the keys represent variable base names, they are not allowed to end
-- with digits.
type BindSet = HashMap Text Int

-- | Return an unused variable name from the given base name
--
-- The returned 'BindSet' includes the new variable.
freshVar :: Text -> BindSet -> (Text, BindSet)
freshVar v bs = case HashMap.lookup v bs of
  Nothing -> (v, HashMap.insert v 0 bs)
  Just n  -> (v <> fromString (show n), HashMap.insert v (n+1) bs)

-- | Allow intensional interpretation of higher-order constructs
--
-- 'Intensional' is used to obtain instances of HOS classes from their
-- intensional counterparts. For example, given
-- @(`CondExpFO` e, `VarExp` e, `CondIntensional` e)@, we get
-- @`CondExp` (`Intensional` e)@.
--
-- Pairing the interpretation with a 'BindSet' allows generating symbolic
-- variables to inspect higher-order constructs rather than just running them.
newtype Intensional e a = Intensional
  { unIntensional :: (Fold BindSet :×: e) a
  } deriving ( ConstExp
             , NumExp
             , FracExp
             , LogicExp
             , CompareExp
             , CondExpFO
             , ListExpFO
             , TupleExp
             , FieldExp
             , AnnExp ann
             , AssertExp
             )

liftIntensional :: (e a -> e b) -> Intensional e a -> Intensional e b
liftIntensional f (Intensional (bsa :×: ea)) = Intensional (coerce bsa :×: f ea)

liftIntensional2 ::
     (e a -> e b -> e c)
  -> Intensional e a
  -> Intensional e b
  -> Intensional e c
liftIntensional2 f (Intensional (bsa :×: ea)) (Intensional (bsb :×: eb)) =
  Intensional ((coerce bsa <> coerce bsb) :×: f ea eb)

liftIntensional3 ::
     (e a -> e b -> e c -> e d)
  -> Intensional e a
  -> Intensional e b
  -> Intensional e c
  -> Intensional e d
liftIntensional3 f
  (Intensional (bsa :×: ea))
  (Intensional (bsb :×: eb))
  (Intensional (bsc :×: ec)) =
    Intensional (Fold (fold bsa <> fold bsb <> fold bsc) :×: f ea eb ec)

-- | Named variable expressions
--
-- This class is only used to internally to create intensional interpretations.
-- It should not be exposed to the EDSL user.
class VarExp e where
  -- | Create a named variable
  varE ::
       DinoType a
    => Text -- ^ Variable name
    -> e a

instance VarExp e => VarExp (Intensional e) where
  varE v = Intensional (mempty :×: varE v)

-- | Open up a binder represented as a Haskell function
--
-- This function helps creating intensional interpretations of higher-order
-- constructs.
unbind ::
     (VarExp e, DinoType a)
  => Text                                 -- ^ Variable base name
  -> (Intensional e a -> Intensional e b) -- ^ Body parameterized by its free variable
  -> (Text, Intensional e b)              -- ^ Generated variable and function body
unbind base f = (v, Intensional (Fold bsb' :×: eb))
  where
    Intensional (Fold bsb :×: eb) = f (varE v)
    (v, bsb') = freshVar base bsb
  -- This function uses the technique described in
  -- "Using Circular Programming for Higher-Order Syntax"
  -- <https://emilaxelsson.github.io/documents/axelsson2013using.pdf>

-- | A version of 'unbind' for 2-argument functions
unbind2 ::
     (VarExp e, DinoType a, DinoType b)
  => Text -- ^ Variable base name
  -> (Intensional e a -> Intensional e b -> Intensional e c)
       -- ^ Body parameterized by its free variables
  -> (Text, Text, Intensional e c) -- ^ Generated variables and function body
unbind2 base f = (va, vb, Intensional (Fold bsc'' :×: ec))
  where
    Intensional (Fold bsc :×: ec) = f (varE va) (varE vb)
    (va, bsc')  = freshVar base bsc
    (vb, bsc'') = freshVar base bsc'

-- | Intensional counterpart of 'CondExp'
class CondIntensional e where
  -- | Intensional counterpart of 'maybe'
  maybeI ::
       DinoType a
    => Text -- ^ Variable name
    -> e b
    -> e b -- ^ Result when 'just' (open term)
    -> e (Maybe a)
    -> e b

-- | Intensional counterpart of 'ListExp'
class ListIntensional e where
  -- | Intensional counterpart of 'mapE'
  mapI ::
       DinoType a
    => Text -- ^ Variable name
    -> e b -- ^ Body (open term)
    -> e [a]
    -> e [b]

  -- | Intensional counterpart of 'dropWhileE'
  dropWhileI ::
       DinoType a
    => Text   -- ^ Name of element variable
    -> e Bool -- ^ Predicate body (open term)
    -> e [a]
    -> e [a]

  -- | Intensional counterpart of 'foldE'
  foldI
    :: (DinoType a, DinoType b)
    => Text -- ^ Name of state variable
    -> Text -- ^ Name of element variable
    -> e a  -- ^ Body (term with two free variables)
    -> e a
    -> e [b]
    -> e a

-- | Intensional counterpart of 'LetExp'
class LetIntensional e where
  -- | Intensional counterpart of 'letE'
  letI ::
       DinoType a
    => Text -- ^ Variable name
    -> e a
    -> e b -- ^ Body (open term)
    -> e b

instance (CondExpFO e, VarExp e, CondIntensional e) =>
         CondExp (Intensional e) where
  maybe n j = liftIntensional3 (maybeI var) n body
    where
      (var, body) = unbind "elem" j

instance (ListExpFO e, VarExp e, ListIntensional e) =>
         ListExp (Intensional e) where
  mapE f = liftIntensional2 (mapI var) body
    where
      (var, body) = unbind "elem" f

  dropWhileE f = liftIntensional2 (dropWhileI var) body
    where
      (var, body) = unbind "elem" f

  foldE f = liftIntensional3 (foldI va vb) body
    where
      (va, vb, body) = unbind2 "elem" f

instance (VarExp e, LetIntensional e) => LetExp (Intensional e) where
  letE base a f = liftIntensional2 (letI var) a body
    where
      (var, body) = unbind base f



--------------------------------------------------------------------------------
-- * AST reification
--------------------------------------------------------------------------------

-- | Generic representation of numbers using 'Rational'
newtype NumRep = NumRep {unNumRep :: Rational}
  deriving (Eq, Ord, Num, Fractional, Real, Hashable)

-- | Integers are show exactly, non-integers are shown at 'Double' precision.
instance Show NumRep where
  show (NumRep n)
    | denominator n Prelude.== 1 = show $ numerator n
    | otherwise = show $ fromRational @Double n

-- | Expression reified as an 'AST'
newtype Reified a = Reified {unReified :: AST NumRep}

instance Inspectable (Reified a) where
  inspect = coerce

appReified :: Constr -> Reified a -> Reified b
appReified con = coerce $ \a -> App @NumRep con [a]

appReified2 :: Constr -> Reified a -> Reified b -> Reified c
appReified2 con = coerce $ \a b -> App @NumRep con [a, b]

appReified3 :: Constr -> Reified a -> Reified b -> Reified c -> Reified d
appReified3 con = coerce $ \a b c -> App @NumRep con [a, b, c]

appReified4 ::
     Constr -> Reified a -> Reified b -> Reified c -> Reified d -> Reified e
appReified4 con = coerce $ \a b c d -> App @NumRep con [a, b, c, d]

appReified5 ::
     Constr
  -> Reified a
  -> Reified b
  -> Reified c
  -> Reified d
  -> Reified e
  -> Reified f
appReified5 con = coerce $ \a b c d f -> App @NumRep con [a, b, c, d, f]

instance ConstExp Reified where
  lit = coerce . inspect

instance NumExp Reified where
  add          = appReified2 "add"
  sub          = appReified2 "sub"
  mul          = appReified2 "mul"
  absE         = appReified  "absE"
  signE        = appReified  "signE"
  fromIntegral = appReified  "fromIntegral"
  floor        = appReified  "floor"
  truncate     = appReified  "truncate"
  roundN n     = appReified2 "roundN" (Reified $ Number $ Prelude.fromIntegral n)

instance FracExp Reified where
  fdiv = appReified2 "fdiv"

instance LogicExp Reified where
  not  = appReified  "not"
  conj = appReified2 "conj"
  disj = appReified2 "disj"
  xor  = appReified2 "xor"

instance CompareExp Reified where
  eq  = appReified2 "eq"
  neq = appReified2 "neq"
  lt  = appReified2 "lt"
  gt  = appReified2 "gt"
  lte = appReified2 "lte"
  gte = appReified2 "gte"
  min = appReified2 "min"
  max = appReified2 "max"

instance CondExpFO Reified where
  just = appReified "just"

  cases cs (Otherwise :-> d) =
    partial_cases (cs ++ [Reified (App "Otherwise" []) :-> d])

  partial_cases cs = Reified $ App "cases" $ pure $ App List
    [App ":->" [unReified c, unReified a] | c :-> a <- cs]

instance ListExpFO Reified where
  range  = appReified2 "range"
  list   = coerce $ App @NumRep List
  headE  = appReified "headE"
  append = appReified2 "append"

instance TupleExp Reified where
  pair = appReified2 "pair"
  fstE = appReified  "fstE"
  sndE = appReified  "sndE"

-- | Field name prepended with @#@
instance FieldExp Reified where
  getField f = appReified (fromString $ "#" ++ symbolVal f)

instance AnnExp Text Reified where
  ann = appReified . Named Annotation

-- | Ignores the assertion
instance AssertExp Reified

instance VarExp Reified where
  varE v = Reified $ App (Named LocalVar v) []

instance CondIntensional Reified where
  maybeI v = appReified3 $ Named Constructor $ "maybe *" <> v

instance ListIntensional Reified where
  mapI v       = appReified2 $ Named Constructor $ "mapE *" <> v
  dropWhileI v = appReified2 $ Named Constructor $ "dropWhileE *" <> v
  foldI va vb  = appReified3 $
    Named Constructor $ Text.unwords ["foldE", "*" <> va, "*" <> vb]

instance LetIntensional Reified where
  letI v a b = (coerce $ Let @NumRep v) a b



--------------------------------------------------------------------------------
-- * Evaluation with variables
--------------------------------------------------------------------------------

-- | Interpretation wrapper that evaluates using a variable environment rather
-- than piggybacking on higher-order syntax
--
-- 'EvalEnv' lacks instances of HOS classes. Instead, it provides instances of
-- 'intensional classes. In order to regain the missing HOS instances, 'EvalEnv'
-- 'can be wrapped in 'Intensional'.
--
-- @`Intensional` (`EvalEnv` e)@ is essentially equivalent to @e@, when @e@ is a
-- 'Monad'.
--
-- The purpose of 'EvalEnv' is to be used when evaluation must be done under
-- 'Intensional'. For example, this happens when combining reification and
-- evaluation.
newtype EvalEnv e a = EvalEnv
  { unEvalEnv :: ReaderT (HashMap Text Dinamic) e a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadReader (HashMap Text Dinamic)
             , ConstExp
             , NumExp
             , FracExp
             , LogicExp
             , CompareExp
             , CondExpFO
             , ListExpFO
             , TupleExp
             , FieldExp
             , AnnExp ann
             )
  -- It would be possible to derive instances of `CondExp`, etc. However, that
  -- would be confusing, since `EvalEnv` is intended to be used exactly in
  -- situations where those instances cannot be used (e.g. inside
  -- `Intensional`).

-- | Add a local variable binding
extendEnv ::
     (Monad e, DinoType a)
  => Text        -- ^ Variable name
  -> a           -- ^ Value of variable
  -> EvalEnv e b -- ^ Expression to evaluate in modified environment
  -> EvalEnv e b
extendEnv var = local . HashMap.insert var . Dinamic

data EvalEnvError
  = NotInScope Text
  | TypeError Text
  deriving (Show)

instance Exception EvalEnvError

-- | Throws 'EvalEnvError' when variable is not in scope or has the wrong type
instance Monad e => VarExp (EvalEnv e) where
  varE var = do
    env <- ask
    return $
      Prelude.maybe (throw $ TypeError var) id $
      fromDinamic $
      Prelude.maybe (throw $ NotInScope var) id $ HashMap.lookup var env

instance Monad e => CondIntensional (EvalEnv e) where
  maybeI var n j m = do
    m' <- m
    case m' of
      Nothing -> n
      Just a -> extendEnv var a j

instance Monad e => ListIntensional (EvalEnv e) where
  mapI       var body as = Prelude.mapM (flip (extendEnv var) body) =<< as
  dropWhileI var body as = dropWhileM   (flip (extendEnv var) body) =<< as

  foldI va vb body a bs = do
    a' <- a
    bs' <- bs
    foldM (\aa bb -> extendEnv va aa $ extendEnv vb bb body) a' bs'

instance Monad e => LetIntensional (EvalEnv e) where
  letI var a b = flip (extendEnv var) b =<< a



--------------------------------------------------------------------------------
-- * Checking assertions
--------------------------------------------------------------------------------

data InvalidAssertion
  = InvalidCondition
      { assertionLabel :: Text
      }
  | NotEqual
      { assertionLabel :: Text
      , reference      :: Text
      , actual         :: Text
      }
  deriving (Show)

instance Exception InvalidAssertion

-- | Interpretation wrapper whose 'AssertExp' instance uses 'MonadError'
newtype AssertViaMonadError e a = AssertViaMonadError
  { unAssertViaMonadError :: e a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadError exc
             , ConstExp
             , NumExp
             , FracExp
             , LogicExp
             , CompareExp
             , CondExpFO
             , CondExp
             , ListExpFO
             , ListExp
             , TupleExp
             , LetExp
             , FieldExp
             , AnnExp ann
             )

-- | Throws 'InvalidAssertion' in the underlying monad
instance MonadError InvalidAssertion e =>
         AssertExp (AssertViaMonadError e) where
  assert lab cond a = do
    c <- cond
    unless c $ throwError $ InvalidCondition lab
    a

  assertEq lab ref act = do
    r <- ref
    a <- act
    unless (r Prelude.== a) $
      throwError $ NotEqual lab (Text.pack $ show r) (Text.pack $ show a)
    return a

-- | Interpretation wrapper whose 'AssertExp' instance uses 'MonadThrow'
newtype AssertViaMonadThrow e a = AssertViaMonadThrow
  { unAssertViaMonadThrow :: e a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadThrow
             , ConstExp
             , NumExp
             , FracExp
             , LogicExp
             , CompareExp
             , CondExpFO
             , CondExp
             , ListExpFO
             , ListExp
             , TupleExp
             , LetExp
             , FieldExp
             , AnnExp ann
             )

-- | Throws 'InvalidAssertion' in the underlying monad
instance MonadThrow e => AssertExp (AssertViaMonadThrow e) where
  assert lab cond a = do
    c <- cond
    unless c $ throwM $ InvalidCondition lab
    a

  assertEq lab ref act = do
    r <- ref
    a <- act
    unless (r Prelude.== a) $
      throwM $ NotEqual lab (Text.pack $ show r) (Text.pack $ show a)
    return a

data Assertion e where
  Assert   :: e Bool -> Assertion e
  AssertEq :: (Eq a, Show a) => e a -> e a -> Assertion e

-- | Collect all assertions in an expression
--
-- Note that the wrapped interpretation @e@ must have instances of intensional
-- classes in order for 'CollectAssertions' to derive instances of HOS classes.
-- In order for the wrapped interpretation to do monadic evaluation, use the
-- wrapper 'EvalEnv' to obtain the necessary intensional instances.
newtype CollectAssertions e a = CollectAssertions
  { unCollectAssertions :: (Intensional (e :×: Fold [(Text, Assertion e)])) a
  } deriving ( ConstExp
             , NumExp
             , FracExp
             , LogicExp
             , CompareExp
             , CondExpFO
             , CondExp
             , ListExpFO
             , ListExp
             , TupleExp
             , LetExp
             , FieldExp
             , AnnExp ann
             , VarExp
             )
  -- TODO Use `Seq` instead of list.

instance AssertExp (CollectAssertions e) where
  assert lab =
    coerce $
    liftIntensional2 @(e :×: Fold [(Text, Assertion e)]) $
      \(c :×: _) (a :×: as) -> (a :×: (as <> Fold [(lab, Assert c)]))

  assertEq lab =
    coerce $
    liftIntensional2 @(e :×: Fold [(Text, Assertion e)]) $
      \(ref :×: _) (act :×: as) ->
        (act :×: (as <> Fold [(lab, AssertEq ref act)]))

-- Here is an example of how to "run" `CollectAssertions`:
--
--     collectAssertions ::
--          (ConstExp e, NumExp e, CompareExp e, VarExp e, LetIntensional e)
--       => (forall e'. ( ConstExp e'
--                      , NumExp e'
--                      , CompareExp e'
--                      , AssertExp e'
--                      , LetExp e'
--                      ) =>
--                        Exp e' a
--          )
--       -> [(Text, Assertion e)]
--     collectAssertions =
--       fold . prodSnd . prodSnd . unIntensional . unCollectAssertions . unExp
--
-- Note that the interpretation type is different in the argument and the
-- result. But the two are related in the following way:
--
--   * FOS classes that appear as constraints on `e'` must also appear as
--     constraints on `e`.
--   * Each HOS class constraint on `e'` requires a corresponding intensional
--     class constraint on `e`.
--
-- Note that the `AssertExp` constraint on `e'` isn't needed on `e`.
