module Semantics.Desugar.DesugarBlocks where

import GrammarTypes.AnsibleGrammarTypes
import Data.List.NonEmpty (toList)

-- desugarTaskForUseWithBlocks :: TH TaskMarker -> Reader (TH TaskMarker, TH TaskMarker) [TH TaskMarker]
-- desugarTaskForUseWithBlocks (ContainingBlock attSet (Block _blockMain _rescue _always)) = let
    