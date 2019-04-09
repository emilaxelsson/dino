{-# OPTIONS_GHC -Wno-missing-signatures #-}

import Dino
import Dino.Verification

import qualified README

exp1 :: (ConstExp e, NumExp e, LetExp e, AssertExp e) => Exp e Int
exp1 =
  ( share (a + b) $ \c ->
      assertEq "should_succeed" ((a + 3) * c) (b * c)
        -- Note: `c` is a free variable in the assertion
  ) + b
  where
    a = assertEq "should_fail" (1+1) (1+2)
    b = a + 3

test1 = presentStructuralVerification $ verifyAssertEqStructurally $ unExp exp1

main = do
  README.tests
  test1
