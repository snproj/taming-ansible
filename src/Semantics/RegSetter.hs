{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
module Semantics.RegSetter where

import GrammarTypes.AnsibleGrammarTypes
import Data.List.NonEmpty (fromList, toList)

class REGSettable a where
    setReg :: a -> a

instance REGSettable (TH a) where
    setReg :: TH a -> TH a
    setReg (Atomic attSet modDecl (SetUID s)) =
        let reg = case kwRegister attSet of
                Just existingRegisterName -> Just existingRegisterName
                Nothing -> Just $ s ++ "_reg"
        in Atomic attSet {kwRegister = reg} modDecl (SetUID s)
    setReg (Atomic _ _ UnsetUID) = error "ERROR: Tried to assign register to Atomic, but Atomic did not yet have UID set!"
    setReg (ContainingBlock attSet (Block _blockMain _rescue _always) uid) = let
        f = fromList . map setReg . toList
        _blockMain' = f _blockMain
        _rescue' = fmap f _rescue
        _always' = fmap f _always
        in ContainingBlock attSet (Block _blockMain' _rescue' _always') uid

instance REGSettable Play where
    setReg :: Play -> Play
    setReg p = let
        _tasks = fmap (map setReg) (tasks p)
        _handlers = fmap (map setReg) (handlers p)
        in p {tasks=_tasks, handlers=_handlers}



