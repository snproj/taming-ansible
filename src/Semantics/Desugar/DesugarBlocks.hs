module Semantics.Desugar.DesugarBlocks where

import GrammarTypes.AnsibleGrammarTypes
import Data.List.NonEmpty (toList)

desugarBlocks :: Block a -> [TH a]
desugarBlocks (Block neTH mRescue) = let
    thList = toList neTH
    in undefined
