module Dino.Verification where

import Prelude

import Data.Text (Text)
import qualified Data.Text as Text
import Text.PrettyPrint.ANSI.Leijen (Doc)
import qualified Text.PrettyPrint.ANSI.Leijen as PP

import Dino.AST.Diff
import Dino.Interpretation

-- | Check each 'assertEq' assertion by structurally comparing the ASTs of the
-- two expressions
--
-- This limited form of formal verification is useful two ways:
--
-- * It can be used to verify refactorings that don't affect the AST; for
--   example, introducing a meta-level helper function (i.e. a normal Haskell
--   function).
--
-- * When the ASTs do differ, the resulting 'Edit' will show only the parts of
--   the expressions where the difference occurs, which can make it easier to
--   manually verify equivalence.
verifyAssertEqStructurally ::
     CollectAssertions Reified a -> [(Text, Maybe (Edit NumRep))]
verifyAssertEqStructurally e =
  [ (lab, diff (unReified ref) (unReified act))
  | (lab, AssertEq ref act) <- as
  ]
  where
    as = fold $ prodSnd $ prodSnd $ unIntensional $ unCollectAssertions e

-- | Present the output of 'verifyAssertEqStructurally' as a list of test cases
-- that either succeed or fail with a diff
presentStructuralVerificationAsDocs :: [(Text, Maybe (Edit NumRep))] -> [Doc]
presentStructuralVerificationAsDocs as = map mkCase as
  where
    l  = maximum (0 : map (Text.length . fst) as)
    l' = min l 20

    showLabel lab =
      Text.unpack lab ++ ":" ++ replicate (l' - Text.length lab) ' '

    mkCase (lab, d) =
      PP.string (showLabel lab) <> PP.space <> diffAsTestResult d <> end
      where
        end = maybe mempty (const $ mempty PP.<$> mempty) d

presentStructuralVerification :: [(Text, Maybe (Edit NumRep))] -> IO ()
presentStructuralVerification =
  PP.putDoc . PP.vsep . presentStructuralVerificationAsDocs
