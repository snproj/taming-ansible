{-# LANGUAGE LambdaCase #-}
module Semantics.Desugar.DesugarHandlers where

import GrammarTypes.AnsibleGrammarTypes
import Semantics.Desugar.DesugarBlocks (updateWhen, updateWhenTH)
import Data.List.NonEmpty (fromList, toList)
import Data.Maybe (fromJust, mapMaybe)
import Data.Map (Map, empty, fromList, unionWith, lookup)

handlerToTask :: TH HandlerMarker -> TH TaskMarker
handlerToTask (Atomic x y z) = Atomic x y z
handlerToTask (ContainingBlock x (Block _blockMain _rescue _always) z) = let
    f = Data.List.NonEmpty.fromList . map handlerToTask . toList
    bm = f _blockMain
    r = fmap f _rescue
    a = fmap f _always
    in ContainingBlock x (Block bm r a) z

getNotifyCond :: TH TaskMarker -> JBE_EXP
getNotifyCond th = let
    reg = (fromJust . atomicRegister . thAtomicAttributeSet) th
    in JBE_EXP_BINARYOP
        (JBE_EXP_REGTEST (JBE_REG_R reg) JBE_TOP_IS JBE_TEST_DEFINED)
        JBE_OP_AND
        (JBE_EXP_REGTEST (JBE_REG_R reg) JBE_TOP_IS JBE_TEST_CHANGED)

varToListOfStrings :: Var -> [String]
varToListOfStrings (ListVar ls) = concatMap varToListOfStrings ls
varToListOfStrings (SimpleVarString s) = [s]
varToListOfStrings _ = error ""

desugarHandlers :: ([TH TaskMarker], [TH HandlerMarker]) -> [TH TaskMarker]
desugarHandlers (tl, hl) = let
    mth = getTaskHandlerMap tl hl
    hl' = map (`updateHandlerWhen` mth) hl
    ht = map handlerToTask hl'
    in tl ++ ht
    where
        getTaskHandlerMap :: [TH TaskMarker] -> [TH HandlerMarker] -> Map (TH HandlerMarker) [TH TaskMarker]
        getTaskHandlerMap ts hs = let
            mlh = getTopicHandlerMap hs
            in getTaskHandlerMap' ts mlh
            where
                getIndivTopicHandlerMap :: TH HandlerMarker -> Map String [TH HandlerMarker]
                getIndivTopicHandlerMap handler = let
                    l = (atomicListen . thAtomicAttributeSet) handler
                    l' = maybe [] varToListOfStrings l
                    l'' = Data.Map.fromList $ map (\s -> (s, [handler])) l'
                    in l''
                getTopicHandlerMap :: [TH HandlerMarker] -> Map String [TH HandlerMarker]
                getTopicHandlerMap hs = let
                    ms = map getIndivTopicHandlerMap hs
                    in foldl (unionWith (++)) empty ms
                getIndivTaskHandlerMap :: TH TaskMarker -> Map String [TH HandlerMarker] -> Map (TH HandlerMarker) [TH TaskMarker]
                getIndivTaskHandlerMap task msh = let
                    n = (atomicNotify . thAtomicAttributeSet) task
                    n' = maybe [] varToListOfStrings n
                    hs = concat $ mapMaybe (`Data.Map.lookup` msh) n'
                    mht = Data.Map.fromList $ map (\h -> (h, [task])) hs
                    in mht
                getTaskHandlerMap' :: [TH TaskMarker] -> Map String [TH HandlerMarker] -> Map (TH HandlerMarker) [TH TaskMarker]
                getTaskHandlerMap' ts msh = let
                    ms = map (`getIndivTaskHandlerMap` msh) ts
                    in foldl (unionWith (++)) empty ms
        updateHandlerWhen :: TH HandlerMarker -> Map (TH HandlerMarker) [TH TaskMarker] -> TH HandlerMarker
        updateHandlerWhen handler mht = let
            ts = fromJust $ Data.Map.lookup handler mht
            in updateHandlerWhen' handler ts
            where
                updateHandlerWhen' :: TH HandlerMarker -> [TH TaskMarker] -> TH HandlerMarker
                updateHandlerWhen' h ts = let
                    aas = thAtomicAttributeSet h
                    VarContainingJinja (JBEPhrase jbe) = atomicWhen aas
                    notifyConds = map getNotifyCond ts
                    jbe' = foldl updateJBE jbe notifyConds
                    newWhen = VarContainingJinja (JBEPhrase jbe')
                    in handler {thAtomicAttributeSet = aas {atomicWhen = newWhen}}
                updateJBE :: JBE_EXP -> JBE_EXP -> JBE_EXP
                updateJBE jbe1 = JBE_EXP_BINARYOP jbe1 JBE_OP_OR


