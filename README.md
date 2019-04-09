# Dino

Dino is a simple [tagless EDSL](okmij.org/ftp/tagless-final) supporting numeric and logic expressions, conditionals, explicit sharing, etc.



Syntactic conveniences
--------------------------------------------------------------------------------

The module [`Dino.Expression`](https://github.com/emilaxelsson/dino/blob/master/src/Dino/Expression.hs) redefines many identifiers from the prelude, so users are advised to hide the prelude when importing it. This can be done, for example, using the `NoImplicitPrelude` language extension. The main module, [`Dino`](https://github.com/emilaxelsson/dino/blob/master/src/Dino.hs), exports both `Dino.Expression` and `Dino.Prelude`, where the latter is a subset of the standard prelude plus a few extra definitions.

Dino provides a newtype wrapper, `Exp`, which allows EDSL terms to be used directly as numbers and strings; for example:

```haskell
ex1 ::
     (ConstExp e, NumExp e, FracExp e, CompareExp e, CondExp e)
  => Exp e Double
  -> Exp e Text
ex1 a =
  if a > 4.5
    then "greater"
    else "smaller or equal"
```

This example also shows the use of `RebindableSyntax` to allow Haskell's `if` syntax in EDSL expressions.

Multi-way conditionals can be expressed using `cases`; for example:

```haskell
beaufortScale ::
     (ConstExp e, NumExp e, FracExp e, CompareExp e, CondExp e)
  => Exp e Double
  -> Exp e Text
beaufortScale v = cases
  [ (v < 0.5)  --> "calm"
  , (v < 13.8) --> "breeze"
  , (v < 24.5) --> "gale" ]
  ( Otherwise  --> "storm" )
```

Browse the [`Dino.Expression`](https://github.com/emilaxelsson/dino/blob/master/src/Dino/Expression.hs) documentation to find different variations on `cases`, including a version for matching on enumerations without a fall-through case.

### A `Maybe`-like monad

Similar to the function `maybe` in the standard prelude, Dino provides the following function to deconstruct `Maybe` values:

```haskell
maybe ::
     (...)
  => e b          -- ^ Result when 'nothing'
  -> (e a -> e b) -- ^ Result when 'just'
  -> e (Maybe a)  -- ^ Value to deconstruct
  -> e b
```

However, it is not very convenient to use `maybe` in a nested way (i.e. when another `maybe` is needed inside the function that handles `just`). In such cases, it is much preferred to use a monadic interface. Indeed, Dino provides the type `Optional` which has a `Monad` instance and the following functions to convert to and from `e (Maybe a)`:

```haskell
suppose     :: (...) => e (Maybe a) -> Optional e (e a)
runOptional :: (...) => Optional e (e a) -> e (Maybe a)
```



Semantic conveniences
--------------------------------------------------------------------------------

On the interpretation side, most Dino constructs provide default implementations for applicative functors, making it easy to derive interpretations for custom applicative functors. There is also special support for intensional analysis of higher-order constructs (i.e. constructs that introduce local variables).

### Standard interpretation with special cases

For example, say that we want standard evaluation of expressions but with the ability to catch division by 0. For this, we can use a newtype around `Maybe`:

```haskell
newtype SafeDiv a = SafeDiv {fromSafeDiv :: Maybe a}
  deriving (Functor, Applicative, Monad)
```

Since `SafeDiv` is an applicative functor, we can derive a standard interpretation of syntactic classes by just declaring instances:

```haskell
instance ConstExp   SafeDiv
instance NumExp     SafeDiv
instance LogicExp   SafeDiv
instance CompareExp SafeDiv
```

(In this particular case, we could also have used `GeneralizedNewtypeDeriving`, since there are already instances of those classes for `Maybe`.)

In order to get special semantics for division, we have to give a manual definition of `fdiv`:

```haskell
instance FracExp SafeDiv where
  fdiv _ (SafeDiv (Just 0)) = SafeDiv Nothing
  fdiv (SafeDiv a) (SafeDiv b) = SafeDiv (liftA2 (/) a b)
```

### Intensional analysis

Dino has special support for intensional interpretation of higher-order constructs.

For example, the class `LetExp` (containing the higher-order `letE` method) has an intensional counterpart `LetIntensional` with the first-order method `letI`. Intensional interpretations (e.g. AST extraction) can be implemented by instantiating `LetIntensional` (and other intensional classes) and then wrap the interpretation in the `Intensional` newtype. `Intensional` automatically derives `LetExp` due to the following instance:

```haskell
instance (VarExp e, LetIntensional e) => LetExp (Intensional e)
```

In other words, you get to *define your semantics using first-order constructs and automatically derive a higher-order interface*.

As an example, the `Reified` interpretation (for AST extraction) is an example of one that only instantiates first-order classes. But the combined type `Intensional Reified` supports a higher-order interface.
