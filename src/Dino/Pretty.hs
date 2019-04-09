{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Helpers for pretty printing
module Dino.Pretty where

import Prelude

import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.List (sortOn)
import Data.String (IsString)
import GHC.Generics (Generic)
import Text.PrettyPrint.ANSI.Leijen (Doc, Pretty (..), (<+>))
import qualified Text.PrettyPrint.ANSI.Leijen as PP

data Importance
  = Unimportant
  | Important
  deriving (Eq, Show, Generic)

-- | Returns 'Important' iff. any argument is 'Important'.
instance Semigroup Importance where
  Unimportant <> Unimportant = Unimportant
  _ <> _ = Important

instance Hashable Importance

-- | Marks a part of a value that hasn't changed
unchanged :: Doc
unchanged = PP.magenta $ PP.text "*"

-- | Emphasize when 'Important'
emphasize :: Importance -> Doc -> Doc
emphasize Unimportant = id
emphasize Important = PP.bold . PP.blue

-- | Place a document indented under a header:
--
-- > header
-- >   doc
-- >   doc
-- >   ...
underHeader ::
     Doc -- ^ Header
  -> Doc -- ^ Document to place under the header
  -> Doc
underHeader h d = h PP.<$> PP.space <+> PP.align d

-- | Render a list of documents as follows:
--
-- > [ a
-- > , b
-- > , ...
-- > ]
--
-- where @'['@, @','@ and @']'@ are provided as the first three parameters.
verticalList :: Doc -> Doc -> Doc -> [Doc] -> Doc
verticalList l _ r [] = l <+> r
verticalList l sep r ds =
  PP.vcat [c <+> PP.align d | (c, d) <- zip (l : repeat sep) ds] PP.<$> r

-- | A wrapper for 'String' with a 'Show' instance that omits quotes
--
-- Useful in situations where 'show' is (ab)used to provide conversion to
-- 'String' rather than for displaying values.
newtype Field = Field {unField :: String}
  deriving (Eq, Ord, IsString, Hashable)

instance Show Field where
  show = unField

instance Pretty Field where
  pretty = PP.string . unField

-- | Render a record as follows:
--
-- > { field1 =
-- >     value1
-- > , field2 =
-- >     value2
-- > ,  ...
-- > }
--
-- If @k@ is a 'String'-like type, it will be shown with quotes. Use 'Field' to
-- prevent this.
prettyRecord :: (Show k, Ord k) => Importance -> HashMap k Doc -> Doc
prettyRecord imp =
  verticalList PP.lbrace PP.comma PP.rbrace .
  map prettyField . sortOn fst . HM.toList
  where
    prettyField (f, v) =
      underHeader (emphasize imp (PP.string (show f)) <+> PP.string "=") v
