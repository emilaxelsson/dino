{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Untyped representation of abstract syntax trees
module Dino.AST
  ( -- * Representation
    Field (..)
  , Mapping (..)
  , NameType (..)
  , Constr (..)
  , Importance (..)
  , AST (..)
  , record
  , prettyNamed

    -- * Generic inspection
  , GInspectableArgs (..)
  , GInspectableFields (..)
  , GInspectable (..)
  , Inspectable (..)
  , inspectListAsRec

    -- * Conversion to Tree
  , toTree
  , showTree
  , drawTree
  , htmlTree
  ) where

import Prelude

import Data.Hashable (Hashable (..))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Proxy (Proxy (..))
import Data.String (IsString(..))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Tree (Tree(..))
import Data.Tree.View (Behavior(..), NodeInfo(..))
import qualified Data.Tree.View as View
import GHC.Generics
  ( (:+:)(..)
  , (:*:)(..)
  , C1
  , D1
  , Generic(..)
  , K1(..)
  , M1(..)
  , Meta(..)
  , Rec0
  , Rep
  , S1
  , U1
  )
import GHC.Stack (HasCallStack)
import GHC.TypeLits (KnownSymbol, symbolVal)
import Text.PrettyPrint.ANSI.Leijen (Doc, Pretty (..))
import qualified Text.PrettyPrint.ANSI.Leijen as PP

import Dino.Pretty



--------------------------------------------------------------------------------
-- * Representation
--------------------------------------------------------------------------------

-- A key-value mapping, used to represent records
--
-- The 'Importance' argument to 'Mapping' is used to distinguish between records
-- whose fields are essentially named parameters and records whose fields carry
-- information.
--
-- For example, a collection of people could be represented as a nested record
-- like this:
--
-- > { Harry = {age = 45, speed = 46}
-- > , Harriet = {age = 47, speed = 48}
-- > , ...
-- > }
--
-- In this case, the outer record can be considered to have 'Important' fields,
-- while the fields in the inner records are just there to give meaning to the
-- numbers.
--
-- But why not just add a @name@ field to the inner records and represent the
-- above collection as a list? The reason why a nested record may be preferred
-- is that it puts the name on the path from the root, which means that it will
-- show up in diffs.
data Mapping k v = Mapping Importance !(HashMap k v)
  deriving (Eq, Show, Foldable, Functor, Traversable, Generic)

instance (Hashable k, Hashable v) => Hashable (Mapping k v) where
  hashWithSalt s (Mapping i m) = hashWithSalt s (i, m)

data NameType
  = Constructor -- ^ Global constructor or variable
  | LocalVar    -- ^ Local variable
  | Annotation  -- ^ User annotation
  deriving (Eq, Show, Generic, Enum, Bounded)

instance Hashable NameType

-- | Description of a constructor or variable
data Constr
  = Tuple
  | List
  | Named NameType Text
  deriving (Eq, Show, Generic)

-- | Creates a 'Named' constructor/variable
instance IsString Constr where
  fromString = Named Constructor . Text.pack

instance Hashable Constr

-- | Representation of abstract syntax and values
--
-- 'AST' is parameterized by the representation of numbers. This makes it
-- possible to affect the exactness of comparisons. For example a newtype with
-- approximate equality can be used instead of e.g. 'Double'.
data AST n
  = Number n                 -- ^ Numeric literal
  | Text Text                -- ^ Text literal
  | App Constr [AST n]       -- ^ Application of constructor or variable
  | Let Text (AST n) (AST n) -- ^ @`Let` v a body@ binds @v@ to @a@ in @body@
  | Record (Mapping Field (AST n))
  deriving (Eq, Show, Foldable, Functor, Traversable, Generic)

instance Hashable n => Hashable (AST n)

record :: HasCallStack => Importance -> [(Field, AST n)] -> AST n
record imp = Record . Mapping imp . HM.fromList

prettyNamed :: NameType -> Text -> Doc
prettyNamed Constructor c = PP.string $ Text.unpack c
prettyNamed LocalVar v    = PP.string $ Text.unpack v
prettyNamed Annotation a  = PP.string $ Text.unpack $ "ANN: " <> a

-- | If @k@ is a 'String'-like type, it will be shown with quotes. Use 'Field'
-- to prevent this.
instance {-# OVERLAPPABLE #-}
         (Pretty a, Show k, Ord k) => Pretty (Mapping k a) where
  pretty (Mapping imp m) = prettyRecord imp $ pretty <$> m

instance Show a => Pretty (AST a) where
  pretty (Number a) = PP.string $ show a
  pretty (Text a) = PP.string $ show a
  pretty (App Tuple []) = PP.parens PP.empty
  pretty (App Tuple vs) =
    verticalList PP.lparen PP.comma PP.rparen $ map pretty vs
  pretty (App List []) = PP.brackets PP.empty
  pretty (App List vs) =
    verticalList PP.lbracket PP.comma PP.rbracket $ map pretty vs
  pretty (App (Named t c) []) = prettyNamed t c
  pretty (App (Named t c) vs) =
    underHeader (prettyNamed t c) $ foldr1 (PP.<$>) $ map pretty vs
  pretty (Let v a b) =
    underHeader (PP.string "let" PP.<+> var PP.<+> "=") (pretty a)
      PP.<$>
    underHeader (PP.string " in") (pretty b)
    where
      var = PP.string $ Text.unpack v
  pretty (Record rec) = pretty rec



--------------------------------------------------------------------------------
-- * Generic inspection
--------------------------------------------------------------------------------

showSym :: forall sym str. (KnownSymbol sym, IsString str) => str
showSym = fromString $ symbolVal (Proxy @sym)

class GInspectableArgs rep where
  gInspectArgs :: rep x -> [AST Rational]

instance GInspectableArgs U1 where
  gInspectArgs _ = []

instance Inspectable a =>
         GInspectableArgs (S1 ('MetaSel 'Nothing x y z) (Rec0 a)) where
  gInspectArgs = pure . inspect . unK1 . unM1

instance (GInspectableArgs rep1, GInspectableArgs rep2) =>
         GInspectableArgs (rep1 :*: rep2) where
  gInspectArgs (rep1 :*: rep2) = gInspectArgs rep1 ++ gInspectArgs rep2

class GInspectableFields rep where
  gInspectFields :: rep x -> [(Field, AST Rational)]

instance GInspectableFields U1 where
  gInspectFields _ = []

instance (Inspectable a, KnownSymbol fld) =>
         GInspectableFields (S1 ('MetaSel ('Just fld) x y z) (Rec0 a)) where
  gInspectFields = pure . (showSym @fld, ) . inspect . unK1 . unM1

instance (GInspectableFields rep1, GInspectableFields rep2) =>
         GInspectableFields (rep1 :*: rep2) where
  gInspectFields (rep1 :*: rep2) = gInspectFields rep1 ++ gInspectFields rep2

class GInspectable rep where
  gInspect :: rep x -> AST Rational

instance (GInspectable rep1, GInspectable rep2) =>
         GInspectable (rep1 :+: rep2) where
  gInspect (L1 rep) = gInspect rep
  gInspect (R1 rep) = gInspect rep

instance GInspectable rep => GInspectable (D1 meta rep) where
  gInspect = gInspect . unM1

instance (GInspectableArgs rep, KnownSymbol con) =>
         GInspectable (C1 ('MetaCons con x 'False) rep) where
  gInspect = App (showSym @con) . gInspectArgs . unM1

instance (GInspectableFields rep, KnownSymbol con) =>
         GInspectable (C1 ('MetaCons con x 'True) rep) where
  gInspect =
    App (showSym @con) .
    pure . Record . Mapping Unimportant . HM.fromList . gInspectFields . unM1

class Inspectable a where
  inspect :: a -> AST Rational

  default inspect :: (Generic a, GInspectable (Rep a)) => a -> AST Rational
  inspect = gInspect . from

instance Inspectable Rational where inspect = Number
instance Inspectable Int      where inspect = Number . toRational
instance Inspectable Integer  where inspect = Number . toRational
instance Inspectable Float    where inspect = Number . toRational
instance Inspectable Double   where inspect = Number . toRational

instance Real n => Inspectable (AST n) where
  inspect = fmap toRational

instance Inspectable () where
  inspect () = App "()" []

instance Inspectable Bool where
  inspect b = App (fromString $ show b) []

instance {-# OVERLAPS #-} Inspectable String where
  inspect = Text . Text.pack

instance Inspectable Text where
  inspect = Text

instance Inspectable a => Inspectable (Maybe a) where
  inspect Nothing  = App "Nothing" []
  inspect (Just a) = App "Just" [inspect a]

instance {-# OVERLAPPABLE #-} Inspectable a => Inspectable [a] where
  inspect = App List . map inspect

instance Inspectable a => Inspectable (Mapping Field a) where
  inspect (Mapping i m) = Record $ Mapping i $ fmap inspect m

instance (Inspectable a, Inspectable b) => Inspectable (a, b) where
  inspect (a, b) = App Tuple [inspect a, inspect b]

instance (Inspectable a, Inspectable b, Inspectable c) =>
         Inspectable (a, b, c) where
  inspect (a, b, c) = App Tuple [inspect a, inspect b, inspect c]

instance (Inspectable a, Inspectable b, Inspectable c, Inspectable d) =>
         Inspectable (a, b, c, d) where
  inspect (a, b, c, d) = App Tuple [inspect a, inspect b, inspect c, inspect d]

-- | Represent a list as a record, if the elements contain a value that can be
-- used as key
inspectListAsRec ::
     Inspectable a
  => Importance
  -> (a -> Field) -- ^ Extract the key
  -> [a]
  -> AST Rational
inspectListAsRec imp getKey as =
  Record $ Mapping imp $ HM.fromList [(getKey a, inspect a) | a <- as]



--------------------------------------------------------------------------------
-- * Conversion to Tree
--------------------------------------------------------------------------------

renderCon :: Constr -> Text
renderCon Tuple = "#Tuple"
renderCon List = "#List"
renderCon (Named t n) = case t of
  Constructor -> n
  LocalVar -> "*" <> n
  Annotation -> "ANN: " <> n

tagTree :: Text -> Tree Text -> Tree Text
tagTree tag (Node n ts) = Node (tag <> n) ts

toTreeRec :: Show n => Mapping Field (AST n) -> [Tree Text]
toTreeRec (Mapping _ fs) =
  [tagTree (Text.pack (unField f) <> ": ") $ toTree a | (f, a) <- HM.toList fs]

-- | Conversion from 'AST' to 'Tree'
--
-- * Built-in consturctors (tuples and lists) are shown prepended with @#@.
--
-- * Record fields are shown as @fieldName:@.
--
-- * Local variables are shown as @*varName@ (both at binding and use site).
--
-- * Annotations are shown as "ANN: annotation ".
toTree :: Show n => AST n -> Tree Text
toTree (App c [Record rec]) = Node (renderCon c) $ toTreeRec rec
toTree (Number n)     = Node (Text.pack $ show n) []
toTree (Text t)       = Node (Text.pack $ show t) []
toTree (App c as)     = Node (renderCon c) $ map toTree as
toTree (Let v a body) = Node ("Let *" <> v) [toTree a, toTree body]
toTree (Record fs)    = Node "Record" $ toTreeRec fs

-- | Show an 'AST' using Unicode art
showTree :: Show n => AST n -> String
showTree = View.showTree . fmap Text.unpack . toTree
  -- TODO Convert `tree-view` to `Text`

-- | Draw an 'AST' on the terminal using Unicode art
drawTree :: Show n => AST n -> IO ()
drawTree = View.drawTree . fmap Text.unpack . toTree

-- | Convert an 'AST' to an HTML file with foldable nodes
htmlTree :: Show n => FilePath -> AST n -> IO ()
htmlTree file =
  View.writeHtmlTree Nothing file . fmap mkInfo . fmap Text.unpack . toTree
  where
    mkInfo n = NodeInfo InitiallyExpanded n ""
