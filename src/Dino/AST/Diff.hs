{-# LANGUAGE UndecidableInstances #-}

module Dino.AST.Diff where

import Prelude

import Control.Monad (guard, zipWithM)
import Data.Coerce (coerce)
import Data.Foldable (asum)
import Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as HM
import Data.List (sortOn)
import Data.Text (Text)
import qualified Data.Text as Text
import Text.PrettyPrint.ANSI.Leijen (Doc, Pretty (..), (<+>))
import qualified Text.PrettyPrint.ANSI.Leijen as PP

import Dino.Pretty
import Dino.AST

-- | Drop elements at the end of a list
dropEnd :: Int -> [a] -> [a]
dropEnd n as = take (length as - n) as



--------------------------------------------------------------------------------
-- * Types
--------------------------------------------------------------------------------

data Replace a = Replace
  { original :: a
  , new :: a
  } deriving (Eq, Show, Functor)

-- | Edit operations on an optional element
data ElemOp a
  = AddElem a
  | RemoveElem a
  | EditElem (Diff a)

deriving instance (Eq a, Eq (Diff a))     => Eq (ElemOp a)
deriving instance (Show a, Show (Diff a)) => Show (ElemOp a)

-- | Edit operations at the end of a list
data EndOp a
  = Append [a]
  | DropEnd [a]
  deriving (Eq, Show, Functor)

-- | Edit operations on lists
data ListOp a =
  ListOp
    [Maybe (Diff a)]
      -- Edits for elements that are common in both lists (drawn from start)
    (Maybe (EndOp a))
      -- Elements that are added or removed at the end

deriving instance (Eq a, Eq (Diff a))     => Eq (ListOp a)
deriving instance (Show a, Show (Diff a)) => Show (ListOp a)

-- | Edit operation on a 'AST'
data Edit a
  = Replacement (Replace (AST a))
  | EditApp Constr [Maybe (Edit a)]
  | EditList (Diff [AST a])
  | EditLet (Diff (Text, AST a, AST a))
  | EditRecord (Diff (Mapping Field (AST a)))
  deriving (Eq, Show)

-- | Wrapper for values that should be regarded as monolithic when diffing
newtype Monolithic a = Monolithic {unMonolithic :: a}



--------------------------------------------------------------------------------
-- * Diffing
--------------------------------------------------------------------------------

class Diffable a where
  -- | Representation of the difference between two values
  type Diff a
  type instance Diff a = Replace a

  -- | Calculate the difference between two values
  --
  -- The result is 'Nothing' iff. the two values are equal.
  --
  -- The following property holds:
  --
  -- @
  -- If   Just d = diff a b
  -- Then Just b = `applyDiff` d a
  -- @
  diff ::
       a -- ^ Original
    -> a -- ^ New
    -> Maybe (Diff a)

  default diff :: (Eq a, Diff a ~ Replace a) => a -> a -> Maybe (Diff a)
  diff original new = do
    guard (original /= new)
    return $ Replace {original, new}

  -- | Apply an 'Edit' to a 'Value'
  --
  -- This function is mostly intended for testing. It succeeds iff. the edit
  -- makes sense.
  applyDiff :: Diff a -> a -> Maybe a

  default applyDiff :: (Eq a, Diff a ~ Replace a) => Diff a -> a -> Maybe a
  applyDiff (Replace {original, new}) a
    | a == original = Just new
    | otherwise = Nothing

applyDiffWhen :: Diffable a => Maybe (Diff a) -> a -> Maybe a
applyDiffWhen Nothing a  = Just a
applyDiffWhen (Just d) a = applyDiff d a

instance Diffable ()
instance Diffable Bool
instance Diffable Text
instance Diffable Int
instance Diffable Integer
instance Diffable Float
instance Diffable Double
instance Diffable Rational

instance Eq a => Diffable (Monolithic a) where
  type Diff (Monolithic a) = Replace a
  diff (Monolithic original) (Monolithic new) = do
    guard (original /= new)
    return $ Replace {original, new}

  applyDiff (Replace {original, new}) a
    | unMonolithic a == original = Just $ Monolithic new
    | otherwise = Nothing

instance Diffable a => Diffable (Maybe a) where
  type Diff (Maybe a) = ElemOp a
  diff Nothing   Nothing   = Nothing
  diff (Just a') Nothing   = Just $ RemoveElem a'
  diff Nothing   (Just b') = Just $ AddElem b'
  diff (Just a') (Just b') = EditElem <$> diff a' b'

  applyDiff (RemoveElem _) Nothing  = Nothing
  applyDiff (RemoveElem _) (Just _) = Just Nothing
  applyDiff (AddElem a) Nothing     = Just (Just a)
  applyDiff (AddElem _) (Just _)    = Nothing
  applyDiff (EditElem d) (Just a)   = Just <$> applyDiff d a
  applyDiff (EditElem _) Nothing    = Nothing

-- | Matches element-wise from the start of the lists, and detects
-- additions/removals at the end.
instance Diffable a => Diffable [a] where
  type Diff [a] = ListOp a
  diff o n
    | Nothing <- asum es, Nothing <- endOp = Nothing
    | otherwise = Just $ ListOp es endOp
    where
      es = zipWith diff o n
      lo = length o
      ln = length n
      endOp
        | lo < ln = Just $ Append (drop lo n)
        | ln < lo = Just $ DropEnd (dropEnd ln o)
        | otherwise = Nothing

  applyDiff (ListOp es endOp) as
    | le < la, maybe False isAppend endOp = Nothing
    | le <= la = applyEndOp <$> zipWithM applyDiffWhen es as
    | otherwise = Nothing
    where
      le = length es
      la = length as
      isAppend (Append _) = True
      isAppend _ = False
      applyEndOp = case endOp of
        Just (Append bs) -> (++ bs)
        _ -> id -- Dropping is handled by `zipWithM` above

instance (Diffable a, Diffable b) => Diffable (a, b) where
  type Diff (a, b) = (Maybe (Diff a), Maybe (Diff b))
  diff (oa, ob) (na, nb)
    | Nothing <- da, Nothing <- db = Nothing
    | otherwise = Just (da, db)
    where
      da = diff oa na
      db = diff ob nb

  applyDiff (da, db) (a, b) = (,) <$> applyDiffWhen da a <*> applyDiffWhen db b

instance (Diffable a, Diffable b, Diffable c) => Diffable (a, b, c) where
  type Diff (a, b, c) = (Maybe (Diff a), Maybe (Diff b), Maybe (Diff c))
  diff (oa, ob, oc) (na, nb, nc)
    | Nothing <- da, Nothing <- db, Nothing <- dc = Nothing
    | otherwise = Just (da, db, dc)
    where
      da = diff oa na
      db = diff ob nb
      dc = diff oc nc

  applyDiff (da, db, dc) (a, b, c) =
    (,,) <$> applyDiffWhen da a <*> applyDiffWhen db b <*> applyDiffWhen dc c

instance (Eq k, Hashable k, Diffable a) => Diffable (Mapping k a) where
  type Diff (Mapping k a) = Mapping k (ElemOp a)

  diff (Mapping oi o) (Mapping ni n)
    | null e = Nothing
    | otherwise = Just $ Mapping (oi <> ni) e
    where
      e = flip HM.mapMaybeWithKey (HM.union o n) $ \k _ ->
        diff (HM.lookup k o) (HM.lookup k n)

  applyDiff (Mapping imp e) (Mapping _ m) =
    fmap (Mapping imp . HM.union additions . HM.mapMaybe id) $
    HM.traverseWithKey applyElem m
    where
      applyElem k v =
        case HM.lookup k e of
          Nothing -> Just $ Just v
          Just (AddElem _) -> Nothing -- Cannot add an existing element
          Just (RemoveElem _) -> Just Nothing
          Just (EditElem d) -> Just <$> applyDiff d v
      additions =
        flip HM.mapMaybe e $ \d ->
          case d of
            AddElem v -> Just v
            _ -> Nothing

instance Eq a => Diffable (AST a) where
  type Diff (AST a) = Edit a

  diff (App List o) (App List n) = EditList <$> diff o n
  diff (App co os) (App cn ns)
    | co == cn && length os == length ns =
      (\(ListOp es _) -> EditApp co es) <$> diff os ns
        -- We know that `os` and `ns` have the same length, so if `diffList`
        -- returns `Just`, it must mean that at least one element in `es` is
        -- `Just`.
  diff (Let vo o bo) (Let vn n bn) = EditLet <$> diff (vo, o, bo) (vn, n, bn)
  diff (Record o) (Record n) = EditRecord <$> diff o n
  diff o n = Replacement <$> diff (Monolithic o) (Monolithic n)

  applyDiff (Replacement d) a = coerce $ applyDiff d (Monolithic a)
  applyDiff (EditList e) (App List as) = App List <$> applyDiff e as
  applyDiff (EditApp c es) (App c' as)
    | c == c' = App c <$> applyDiff (ListOp es Nothing) as
  applyDiff (EditLet e) (Let v a b) =
    (\(v', a', b') -> Let v' a' b') <$> applyDiff e (v, a, b)
  applyDiff (EditRecord e) (Record rec) = Record <$> applyDiff e rec
  applyDiff _ _ = Nothing



--------------------------------------------------------------------------------
-- * Rendering
--------------------------------------------------------------------------------

-- | If @k@ is a 'String'-like type, it will be shown with quotes. Use 'Field'
-- to prevent this.
instance {-# OVERLAPPING #-}
         (Pretty a, Pretty (Diff a), Show k, Ord k) =>
         Pretty (Mapping k (ElemOp a)) where
  pretty (Mapping imp m) =
    verticalList PP.lbrace PP.comma PP.rbrace $
    map prettyField $ sortOn fst $ HM.toList m
    where
      prettyField (f, AddElem v) =
        PP.green $
        (PP.text "+" <+>
         emphasize imp (PP.string (show f)) <+> PP.text "=") PP.<$>
        PP.text "    " <>
        PP.align (pretty v)
      prettyField (f, RemoveElem v) =
        PP.red $
        (PP.text "-" <+>
         emphasize imp (PP.string (show f)) <+> PP.text "=") PP.<$>
        PP.text "    " <>
        PP.align (pretty v)
      prettyField (f, EditElem e) =
        underHeader (emphasize imp (PP.string (show f)) <+> PP.text "=") $
        pretty e

instance Pretty a => Pretty (Replace a) where
  pretty Replace {original, new} =
    PP.red (PP.char '-' <+> PP.align (pretty original)) PP.<$>
    PP.green (PP.char '+' <+> PP.align (pretty new))

-- | Pretty print for edits on tuple-like collections (where elements are
-- identified by position)
prettyEditTuple :: Pretty a => Doc -> Doc -> Doc -> [Maybe a] -> Doc
prettyEditTuple l sep r = verticalList l sep r . map (maybe unchanged pretty)

-- | Pretty print 'EditApp' for \"named\" constructors
prettyEditApp :: Pretty a => NameType -> Text -> [Maybe a] -> Doc
prettyEditApp t c [] = prettyNamed t c
prettyEditApp t c as =
  underHeader (prettyNamed t c) $ PP.vcat $ map (maybe unchanged pretty) as

instance (Pretty a, Pretty (Diff a)) => Pretty (ListOp a) where
  pretty (ListOp es endOp) =
    verticalList PP.lbracket PP.comma PP.rbracket (es' ++ os)
    where
      es' =
        [ underHeader (PP.magenta $ PP.text ("edit @" <> show i)) (pretty e)
        | (i :: Int, Just e) <- zip [0 ..] es
        ]
      os = case endOp of
        Nothing -> []
        Just (Append vs) ->
          [ underHeader (PP.magenta $ PP.text "append") $
          PP.green (PP.char '+' <+> PP.align (pretty v))
          | v <- vs
          ]
        Just (DropEnd vs) ->
          [ underHeader (PP.magenta $ PP.text "drop from end") $
          PP.red (PP.char '-' <+> PP.align (pretty v))
          | v <- vs
          ]

instance Show a => Pretty (Edit a) where
  pretty (EditApp Tuple es) =
    verticalList PP.lparen PP.comma PP.rparen $ map (maybe unchanged pretty) es
  pretty (Replacement e)          = pretty e
  pretty (EditApp List es)        = pretty $ EditList (ListOp es Nothing)
  pretty (EditApp (Named t c) es) = prettyEditApp t c es
  pretty (EditList e)             = pretty e
  pretty (EditLet (v, a, b)) =
    underHeader (PP.string "let" PP.<+> PP.align var PP.<+> "=") (maybe unchanged pretty a)
      PP.<$>
    underHeader (PP.string " in") (maybe unchanged pretty b)
    where
      var = maybe unchanged (pretty . fmap (PP.string . Text.unpack)) v
        -- TODO Would maybe be good to show the variable name even if it hasn't
        -- changed...
  pretty (EditRecord (Mapping imp erec)) =
    verticalList PP.lbrace PP.comma PP.rbrace $
    map prettyField $ sortOn fst $ HM.toList erec
    where
      prettyField (f, AddElem v) =
        PP.green $
        (PP.text "+" <+>
         emphasize imp (PP.string (unField f)) <+> PP.text "=") PP.<$>
        PP.text "    " <>
        PP.align (pretty v)
      prettyField (f, RemoveElem v) =
        PP.red $
        (PP.text "-" <+>
         emphasize imp (PP.string (unField f)) <+> PP.text "=") PP.<$>
        PP.text "    " <>
        PP.align (pretty v)
      prettyField (f, EditElem e) =
        underHeader (emphasize imp (PP.string (unField f)) <+> PP.text "=") $
        pretty e

-- | Print an 'Edit' value to the terminal using ANSI colors
printEdit :: Show a => Edit a -> IO ()
printEdit e = PP.putDoc (pretty e) >> putStrLn ""

-- | Print a diff as a test result
--
-- 'Nothing' is shown as a green \"OK\".
--
-- @`Just` d@ is shown as a red \"Fail\", followed by a rendering of the diff.
diffAsTestResult :: Show a => Maybe (Edit a) -> Doc
diffAsTestResult Nothing  = PP.green $ PP.string "OK"
diffAsTestResult (Just e) = underHeader (PP.red (PP.string "Fail")) (PP.pretty e)
