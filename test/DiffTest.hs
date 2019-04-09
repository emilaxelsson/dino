{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module DiffTest where

import Prelude

import Control.Monad (replicateM)
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromJust, isJust, isNothing)
import Data.Text (Text)
import qualified Data.Text as Text

import Dino.Pretty
import Dino.AST
import Dino.AST.Diff hiding (new)

import Test.Tasty.QuickCheck
import Test.Tasty.TH

instance Arbitrary NameType where
  arbitrary = oneof $ map pure [minBound .. maxBound]

-- | Generate a short text consisting of alphabetic characters
instance Arbitrary Text where
  arbitrary = do
    n <- choose (1,6)
    fmap Text.pack $ vectorOf n $ choose ('a', 'z')

  shrink "v" = []
  shrink _   = ["v"]

instance Arbitrary Field where
  arbitrary = do
    n <- choose (1,6)
    fmap Field $ vectorOf n $ choose ('a', 'z')

instance Arbitrary Constr where
  arbitrary = frequency
    [ (1, return List)
    , (1, return Tuple)
    , (3, Named <$> arbitrary <*> arbitrary)
    ]

  shrink List = []
  shrink _    = [List]

instance Arbitrary a => Arbitrary (Mapping Field a) where
  arbitrary = sized $ \s -> do
    n <- choose (0, s)
    Mapping Unimportant . HM.fromList <$>
      replicateM n ((,) <$> arbitrary <*> resize (s `div` n) arbitrary)

-- | Generate an 'AST' of bounded size
genAST :: Arbitrary a => Int -> Gen (AST a)
genAST s =
  frequency
    [ (1, Number <$> arbitrary)
    , (1, Text <$> arbitrary)
    , (s, ) $ do
        n <- choose (0, s)
        App <$> arbitrary <*> replicateM n (genAST (s `div` n))
    , (s, ) $ Let <$> arbitrary <*> genAST (s `div` 2) <*> genAST (s `div` 2)
    , (s, ) . fmap Record $ resize s arbitrary
    ]

shrinkAST :: Arbitrary a => AST a -> [AST a]
shrinkAST (Number _) = [Text ""]
shrinkAST (Text _) = []
shrinkAST (App c as) = as ++ map (uncurry App) (shrink (c, as))
shrinkAST (Let v a b) =
  [a, b] ++ [Let v' a' b' | (a', b') <- shrink (a, b), v' <- shrink v]
shrinkAST (Record (Mapping imp rec)) =
  HM.elems rec ++
  [App List $ HM.elems rec] ++
  map
    (Record . Mapping imp . HM.fromList)
    (shrinkList (\(f, a) -> map (f, ) $ shrink a) $ HM.toList rec)

instance Arbitrary a => Arbitrary (AST a) where
  arbitrary = sized genAST
  shrink = shrinkAST

oldOrNew :: Arbitrary a => a -> Gen a
oldOrNew a = frequency [(1, resize 3 arbitrary), (5, return a)]

class ArbitraryRelative a where
  -- | Generate a value by applying small changes to an existing value
  genRelative :: a -> Gen a

  default genRelative :: Arbitrary a => a -> Gen a
  genRelative = oldOrNew

instance (Arbitrary a, ArbitraryRelative a) => ArbitraryRelative [a] where
  genRelative as = do
    as' <- mapM (\a -> frequency [(4, return a), (1, genRelative a)]) as
    frequency
      [ (3, ) $ return as'
      , (1, ) $ do
          n <- choose (0, 2)
          return $ dropEnd n as'
      , (1, ) $ do
          n <- choose (0, 2)
          (as' ++) <$> replicateM n (resize 3 arbitrary)
      ]

instance (Arbitrary a, ArbitraryRelative a) => ArbitraryRelative (Maybe a) where
  genRelative Nothing  = oldOrNew Nothing
  genRelative (Just a) = frequency
    [ (1, return Nothing)
    , (4, Just <$> genRelative a)
    ]

instance (Arbitrary a, ArbitraryRelative a) =>
         ArbitraryRelative (Mapping Field a) where
  genRelative (Mapping imp m) =
    frequency
      [ ( 4
        , do m' <-
               fmap (HM.mapMaybe id) $
               flip traverse m $ \a ->
                 frequency
                   [ (3, Just <$> return a)
                   , (1, Just <$> genRelative a)
                   , (1, return Nothing)
                   ]
             Mapping _ m'' <- resize 3 arbitrary
             return $ Mapping imp $ HM.union m'' m')
      , (1, resize 5 arbitrary)
      ]

instance (ArbitraryRelative a, ArbitraryRelative b) =>
         ArbitraryRelative (a, b) where
  genRelative (a, b) = (,) <$> genRelative a <*> genRelative b

instance ArbitraryRelative Constr

instance Arbitrary a => ArbitraryRelative (AST a) where
  genRelative (Number a) = frequency
    [ (4, return $ Number a)
    , (2, Number <$> arbitrary)
    , (1, genAST 3)
    ]
  genRelative (Text a) = frequency
    [ (4, return $ Text a)
    , (2, Text <$> arbitrary)
    , (1, genAST 3)
    ]
  genRelative (App c as) = frequency
    [ (4, App <$> genRelative c <*> genRelative as)
    , (1, genAST 3)
    ]
  genRelative (Let v a b) = frequency
    [ (4, Let <$> oldOrNew v <*> genRelative a <*> genRelative b)
    , (1, genAST 3)
    ]
  genRelative (Record rec) = frequency
    [ (4, Record <$> genRelative rec)
    , (1, genAST 3)
    ]

genEdit :: Int -> Gen (Edit Int)
genEdit s = do
  ast <- genAST s
  rel <- genRelative ast
  maybe (genEdit s) return $ diff ast rel

-- | If the edit is empty, then @ast@ and @rel@ are equal
prop_diffEq1 =
  forAllShrink (genAST 8) shrinkAST $ \(a :: AST Int) ->
    forAllShrink (genRelative a) shrinkAST $ \rel ->
      isNothing (diff a rel) ==> a == rel

-- | If @ast@ and @rel@ are equal, then the edit is empty
prop_diffEq2 =
  forAllShrink (genAST 8) shrinkAST $ \(a :: AST Int) ->
    forAllShrink (genRelative a) shrinkAST $ \rel ->
      a == rel ==> isNothing (diff a rel)

-- | Applying the calculated edit to the original retrieves the new 'AST'
prop_diffApply (orig :: AST Int) =
  forAllShrink (genRelative orig) shrinkAST $ \new ->
    let e = diff orig new
     in isJust e ==> applyDiff (fromJust e) orig == Just new

-- TODO Test that the diff is "optimal". E.g. don't use `Replacement` when a
-- smaller representation of the diff exists.

-- TODO Property that tests when `applyDiff` must return `Nothing`. Probably
-- hard...

tests = $testGroupGenerator
