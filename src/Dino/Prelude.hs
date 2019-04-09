-- | Prelude for Dino expressions
--
-- This module mostly re-exports the standard "Prelude", but hides identifiers
-- that are overridden by Dino definitions.

module Dino.Prelude
  ( module Prelude
  , HasField
  , Hashable
  , IsLabel
  , KnownSymbol
  , Symbol
  , Text
  , Typeable

    -- Needed when `RebindableSyntax` is enabled:
  , fromLabel
  , fromString
  , join
  ) where

-- Regarding the exports:
--
--   * Dino code and ordinary application code normally live in separate
--     modules. Dino code normally doesn't need to use IO, except maybe for
--     calling a back end and printing results. Hence it only needs a simple
--     prelude.
--
--   * Dino is supposed to be used by non-developers who are maybe Haskell
--     beginners. The standard Prelude seems suitable for them.
--
--   * Prelude deficiencies such as `String` and partial functions are no
--     problem for Dino code, since Haskell's run time is Dino's compile time.

import Prelude hiding
  ( RealFrac(..)
  , (&&)
  , (||)
  , (==)
  , (/=)
  , (<)
  , (>)
  , (<=)
  , (>=)
  , all
  , and
  , any
  , fromIntegral
  , max
  , maybe
  , min
  , not
  , or
  )
import Prelude (RealFrac)

import Control.Monad (join)
import Data.Hashable (Hashable)
import Data.String (fromString)
import Data.Text (Text)
import Data.Typeable (Typeable)
import GHC.OverloadedLabels (IsLabel (..))
import GHC.TypeLits (KnownSymbol, Symbol)
import GHC.Records (HasField)
