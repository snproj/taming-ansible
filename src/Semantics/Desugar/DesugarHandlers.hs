{-# LANGUAGE LambdaCase #-}
module Semantics.Desugar.DesugarHandlers where

import GrammarTypes.AnsibleGrammarTypes
import Semantics.Desugar.DesugarBlocks (updateWhen, updateWhenTH)
import Data.List.NonEmpty (fromList, toList)
import Data.Maybe (fromJust)
import Data.Map (Map, empty, fromList, unionWith)

desugarHandler :: TH HandlerMarker -> TH TaskMarker
desugarHandler (Atomic x y z) = Atomic x y z
desugarHandler (ContainingBlock x (Block _blockMain _rescue _always) z) = let
    f = Data.List.NonEmpty.fromList . map desugarHandler . toList
    bm = f _blockMain
    r = fmap f _rescue
    a = fmap f _always
    in ContainingBlock x (Block bm r a) z

getNotifyCond :: TH TaskMarker -> JBE_EXP
getNotifyCond th = let
    reg = (fromJust . kwRegister . thAttributeSet) th
    in JBE_EXP_BINARYOP
        (JBE_EXP_REGTEST (JBE_REG_R reg) JBE_TOP_IS JBE_TEST_DEFINED)
        JBE_OP_AND
        (JBE_EXP_REGTEST (JBE_REG_R reg) JBE_TOP_IS JBE_TEST_CHANGED)

desugarNotify :: ([TH TaskMarker], [TH HandlerMarker]) -> ([TH TaskMarker], [TH HandlerMarker])
desugarNotify (tl, hl) = let
    
    in undefined
        where
            getTargetMap :: TH HandlerMarker -> Map String [TH HandlerMarker]
            getTargetMap handler = let
                n = (kwName . thAttributeSet) handler
                n' = maybe empty (\s -> Data.Map.fromList [(s, [handler])]) n
                l = (kwListen . thAttributeSet) handler
                l' = maybe empty (\s -> Data.Map.fromList [(s, [handler])]) l
                in unionWith (++) n' l'
            getTargetMaps :: [TH HandlerMarker] -> Map String [TH HandlerMarker]
            getTargetMaps hs = let
                ms = map getTargetMap hs
                in foldl (unionWith (++)) empty ms
            findNotifyTargets :: TH TaskMarker -> [String]
            findNotifyTargets task = case (kwNotify . thAttributeSet) task of
                Nothing -> []
                Just n -> case n of
                        ListVar ls -> map (\(SimpleVarString s) -> s) ls
                        SimpleVarString s -> [s]
            getAllNotifyTargets :: [TH HandlerMarker] ->
