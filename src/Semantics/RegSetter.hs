-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE InstanceSigs #-}
-- module Semantics.RegSetter where

-- import GrammarTypes.AnsibleGrammarTypes
-- import Data.List.NonEmpty (fromList, toList)

-- class REGSettable a where
--     setReg :: a -> a

-- instance REGSettable Task where
--     setReg :: Task -> Task
--     setReg (Atomic attSet modDecl (SetUID s)) =
--         Atomic attSet {atomicRegister = Just s} modDecl (SetUID s)
--         -- let reg = case atomicRegister attSet of
--         --         Just existingRegisterName -> Just existingRegisterName
--         --         Nothing -> Just $ s ++ "_reg"
--         -- in Atomic attSet {atomicRegister = reg} modDecl (SetUID s)
--     setReg (Atomic _ _ UnsetUID) = error "ERROR: Tried to assign register to Atomic, but Atomic did not yet have UID set!"
--     setReg (ContainingBlock attSet (Block _blockMain _rescue _always) uid) = let
--         f = fromList . map setReg . toList
--         _blockMain' = f _blockMain
--         _rescue' = fmap f _rescue
--         _always' = fmap f _always
--         in ContainingBlock attSet (Block _blockMain' _rescue' _always') uid

-- instance REGSettable Play where
--     setReg :: Play -> Play
--     setReg p = let
--         _tasks = map setReg (tasks p)
--         _handlers = map setReg (handlers p)
--         in p {tasks=_tasks, handlers=_handlers}



