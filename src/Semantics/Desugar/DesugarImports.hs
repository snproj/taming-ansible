{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE InstanceSigs #-}
module Semantics.Desugar.DesugarImports where

import GrammarTypes.AnsibleH
import Control.Monad.Reader (Reader, ask, local, runReader)
import Data.Map (lookup, union)
import Data.Maybe (fromMaybe, fromJust)
import qualified Data.Map as Map
import Semantics.UIDSetter (setUID)
import Semantics.Desugar.DesugarBlocks (createGoalkeeper)

desugarImportsInPlay :: Play -> Reader RootDir Play
desugarImportsInPlay p = do
    (newTasks, newHandlers) <- desugarImports $ tasks p
    pure p {
        tasks = newTasks,
        handlers = handlers p ++ newHandlers
    }

desugarImports :: [Task] -> Reader RootDir ([Task], [Task])
desugarImports ts = do
    pairs <- mapM getImportsFromTask ts
    let (expandedTasks, expandedHandlers) = unzip pairs
    return (concat expandedTasks, concat expandedHandlers)

getImportsFromTask :: Task -> Reader RootDir ([Task], [Task])
getImportsFromTask t = case t of
    Atomic {} -> case modDecl t of
        GenericModDecl {} -> return ([t],[])
        ImportRole {} -> do
            tasksFromRole <- getTasksFromRole (name (modDecl t)) (tasksFrom (modDecl t))
            handlersFromRole <- getHandlersFromRole (name (modDecl t)) (handlersFrom (modDecl t))
            let tasksFromRole' = map (atomicParAtts (atomicAttributeSet t)) tasksFromRole
            let handlersFromRole' = map (atomicParAtts (atomicAttributeSet t)) handlersFromRole
            return (tasksFromRole', handlersFromRole')
        ImportTasks f -> do
            tasksFromFile <- getTasksFromFile f
            let tasksFromFile' = map (atomicParAtts (atomicAttributeSet t)) tasksFromFile
            return (tasksFromFile', [])
    Blocktask {} -> do
        (ebm, handlersFromBM) <- desugarImports (blockMain (block t))
        (er, handlersFromR) <- desugarImports (rescue (block t))
        (ea, handlersFromA) <- desugarImports (always (block t))
        let newBlock = Block {
            blockMain = ebm,
            rescue = er,
            always = ea,
            goalkeeper = Nothing
        }
        let newBlockTask = Blocktask {
            blockAttributeSet = blockAttributeSet t,
            block = newBlock,
            uid = UnsetUID
        }
        let handlersFromBlock = map (blockParAtts (blockAttributeSet t)) (handlersFromBM ++ handlersFromR ++ handlersFromA)
        return ([newBlockTask], handlersFromBlock)



atomicParAtts :: AtomicAttributeSet -> Task -> Task
atomicParAtts a t = let
    modifiedAttSet = (atomicAttributeSet t) {
        atomicNotify = atomicNotify a ++ atomicNotify (atomicAttributeSet t),
        atomicWhen = JBE_EXP_BINARYOP (atomicWhen a) JBE_OP_AND (atomicWhen (atomicAttributeSet t)),
        atomicVars = Data.Map.union (atomicVars a) (atomicVars (atomicAttributeSet t))
    }
    in t {atomicAttributeSet=modifiedAttSet}

blockParAtts :: BlockAttributeSet -> Task -> Task
blockParAtts b t = let
    modifiedAttSet = (atomicAttributeSet t) {
        atomicNotify = blockNotify b ++ atomicNotify (atomicAttributeSet t),
        atomicVars = Data.Map.union (blockVars b) (atomicVars (atomicAttributeSet t))
    }
    in t {atomicAttributeSet=modifiedAttSet}



getTasksFromRole :: Var -> Var -> Reader RootDir [Task]
getTasksFromRole (SimpleVarString n) (SimpleVarString tf) = do
    rd <- ask
    let r = fromJust $ Data.Map.lookup n (roledir rd)
    let t = fromJust $ Data.Map.lookup tf (tasksDir r)
    return t

getHandlersFromRole :: Var -> Var -> Reader RootDir [Task]
getHandlersFromRole (SimpleVarString n) (SimpleVarString tf) = do
    rd <- ask
    let r = fromJust $ Data.Map.lookup n (roledir rd)
    let t = fromJust $ Data.Map.lookup tf (handlersDir r)
    return t

getTasksFromFile :: Var -> Reader RootDir [Task]
getTasksFromFile (SimpleVarString f) = do
    rd <- ask
    let t = fromJust $ Data.Map.lookup f (looseTaskFiles rd)
    return t

rewriteRuleImports :: RootDir -> RootDir
rewriteRuleImports rd = let
    ps' = map (\p -> runReader (desugarImportsInPlay p) rd) (playbook rd)
    in rd {
        playbook = ps'
    }