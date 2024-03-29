{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RebindableSyntax #-}

{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- Code from README.md

module README where

import qualified Prelude

import Control.Applicative (liftA2)

import Dino
import Dino.AST (drawTree)
import Dino.Interpretation



--------------------------------------------------------------------------------
-- * Syntax
--------------------------------------------------------------------------------

ex1 ::
     (ConstExp e, NumExp e, FracExp e, CompareExp e, CondExp e)
  => Exp e Double
  -> Exp e Text
ex1 a =
  if a > 4.5
    then "greater"
    else "smaller or equal"

beaufortScale ::
     (ConstExp e, NumExp e, FracExp e, CompareExp e, CondExp e)
  => Exp e Double
  -> Exp e Text
beaufortScale v = cases
  [ (v < 0.5)  --> "calm"
  , (v < 13.8) --> "breeze"
  , (v < 24.5) --> "gale" ]
  ( Otherwise  --> "storm" )

safeDiv ::
     (ConstExp e, FracExp e, CompareExp e, CondExp e, DinoType a, Fractional a)
  => e a
  -> e a
  -> Optional e (e a)
safeDiv a b = suppose $ if (b /= lit 0)
  then just (fdiv a b)
  else nothing

foo ::
     (ConstExp e, NumExp e, FracExp e, CompareExp e, CondExp e, LetExp e)
  => Exp e Double
  -> Exp e Double
  -> Exp e Double
foo a b = fromOptional 0 $ do
  x <- safeDiv a b
  y <- safeDiv b x
  safeDiv x y

-- Let's have look at the expression generated by `foo`:

fooExp = drawTree $ unReified $ prodSnd $ unIntensional $ unExp $ foo 111 222
  -- The type of `foo` here is `Exp (Intensional Reified) Double`



--------------------------------------------------------------------------------
-- * Semantics
--------------------------------------------------------------------------------

newtype SafeDiv a = SafeDiv {fromSafeDiv :: Maybe a}
  deriving (Functor, Applicative, Monad)

instance ConstExp   SafeDiv
instance NumExp     SafeDiv
instance LogicExp   SafeDiv
instance CompareExp SafeDiv

instance FracExp SafeDiv where
  fdiv _ (SafeDiv (Just b))
    | b Prelude.== 0           = SafeDiv Nothing
  fdiv (SafeDiv a) (SafeDiv b) = SafeDiv (liftA2 (/) a b)

evalSafeDiv
  :: (forall e. (ConstExp e, NumExp e, FracExp e, LogicExp e, CompareExp e) => Exp e a)
  -> Maybe a
evalSafeDiv e = fromSafeDiv (unExp e)

exDiv1 :: (ConstExp e, NumExp e, FracExp e, LogicExp e, CompareExp e) => Exp e Double
exDiv1 = 1+2/3

exDiv2 :: (ConstExp e, NumExp e, FracExp e, LogicExp e, CompareExp e) => Exp e Double
exDiv2 = 1+2/0

ex2 ::
     (ConstExp e, NumExp e, CompareExp e, CondExp e, LetExp e)
  => Exp e Double
  -> Exp e Double
ex2 a = letE "x" expensive $ \x ->
  if a > 10
    then x*2
    else x*3
  where
    expensive = a*a*a*a*a*a*a*a



--------------------------------------------------------------------------------

tests = do
  print $ eval $ ex1 2
  print $ eval $ ex1 5
  print $ eval $ beaufortScale 8
  print $ eval $ foo 11 12
  print $ eval $ foo 11 0
  fooExp
  print $ evalSafeDiv exDiv1
  print $ evalSafeDiv exDiv2
