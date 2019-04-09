-- | The Dino language
--
-- Dino is a tagless embedded DSL for numeric simple calculations.

module Dino
  ( module Dino.Prelude
  , module Dino.Types
  , module Dino.Expression
  ) where

import Dino.Prelude
import Dino.Types
import Dino.Expression
import Dino.Interpretation ()
