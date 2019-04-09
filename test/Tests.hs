{-# OPTIONS_GHC -Wno-missing-signatures #-}

import Test.Tasty

import qualified DiffTest

main = defaultMain DiffTest.tests
