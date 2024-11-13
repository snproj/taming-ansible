{-# LANGUAGE LambdaCase #-}
module Semantics.Desugar.DesugarRoles where

import GrammarTypes.AnsibleGrammarTypes
import Data.Map (elems, Map, empty, insert, lookup, map)
import Control.Monad.Reader (Reader, ask, MonadTrans (lift))
import Control.Monad.Trans.Maybe (MaybeT(..))
import Data.Maybe (fromJust, mapMaybe, fromMaybe, isJust)
import Data.List.NonEmpty (filter)
import qualified Data.Map as Map


desugarPlainRoleCalls :: Play -> Reader (Maybe (Map String Role)) Play
desugarPlainRoleCalls (Play hp attSet ts hs rNames) = do
    roles <- ask
    res <- runMaybeT $ do
            msr <- MaybeT (return roles :: Reader (Maybe (Map String Role)) (Maybe (Map String Role)))
            nList <- MaybeT (return rNames)
            let mRoles = Prelude.map (`Data.Map.lookup` msr) nList
            let jRoles = Prelude.map fromJust mRoles -- throw error if undefined role is called

            -- sad code duplication for tasks and handlers
            let taskLists = mapMaybe getMainTasksFromRole jRoles
            let flattenedTaskLists = concat taskLists
            let newTs = do
                    ts'' <- ts
                    return (flattenedTaskLists ++ ts'')
            -- sad code duplication for tasks and handlers
            let handlerLists = mapMaybe getMainHandlersFromRole jRoles
            let flattenedHandlerLists = concat handlerLists
            let newHs = do
                    hs'' <- hs
                    return (flattenedHandlerLists ++ hs'')

            return (Play hp attSet newTs newHs Nothing)
    return (fromMaybe (Play hp attSet ts hs Nothing) res)


getMainTasksFromRole :: Role -> Maybe [TH TaskMarker]
getMainTasksFromRole r = do
    _tasksDir <- tasksDir r
    Map.lookup MainName _tasksDir


-- getTaskFileFromTaskDir :: CompulsoryRoleDir -> RoleSubDirFileName -> Maybe [TH TaskMarker]
-- getTaskFileFromTaskDir (TasksDir mnt) n =  Data.Map.lookup n mnt
-- getTaskFileFromTaskDir _ _ = error "ERROR: Tried to get task file from non-task dir"

-- getMainHandlersFromRole :: Role -> Maybe [TH HandlerMarker]
-- getMainHandlersFromRole (Role neCRD) = do
--   let justHandlerDirs = Data.List.NonEmpty.filter (\case HandlersDir _ -> True; _ -> False) neCRD
--   handlerDir <- case justHandlerDirs of
--     [td] -> Just td
--     [] -> Nothing
--     _ -> error ""
--   getHandlerFileFromHandlerDir handlerDir MainName

getMainHandlersFromRole :: Role -> Maybe [TH HandlerMarker]
getMainHandlersFromRole r = do
    _handlersDir <- handlersDir r
    Map.lookup MainName _handlersDir

-- getHandlerFileFromHandlerDir :: CompulsoryRoleDir -> RoleSubDirFileName -> Maybe [TH HandlerMarker]
-- getHandlerFileFromHandlerDir (HandlersDir mnt) n = Data.Map.lookup n mnt
-- getHandlerFileFromHandlerDir _ _ = error "ERROR: Tried to get handler file from non-handler dir"




-- class Resolvable a where
--     isResolved :: a -> Bool  
--     resolve :: a -> Map String Var -> a

-- isResolvedMaybe :: Resolvable a => Maybe a -> Bool
-- isResolvedMaybe = maybe True isResolved

-- instance Resolvable Task where
--     isResolved t = case t of
--         AtomicTask attSet modDecl -> isResolved attSet && isResolved modDecl
--         TaskContainingABlock attSet blockTask -> isResolved attSet && isResolved blockTask

-- instance Resolvable ModDecl where
--     isResolved (ModDecl _ msv) = all isResolved (elems msv)

-- instance Resolvable BlockTask where
--     isResolved (BlockTask neTask maybeRescueTask) =
--         all isResolved neTask &&
--         isResolvedMaybe maybeRescueTask

-- -- instance Resolvable RescueTask where
-- --     isResolved ()

-- instance Resolvable AttributeSet where
--     isResolved (AttributeSet _kwName _kwForceHandlers _kwNotify _kwLoop _kwWhen _kwVars _kwChangedWhen _kwFailedWhen _kwUntil _kwRetries _kwRegister) =
--         -- isResolvedMaybe _kwName &&
--         isResolvedMaybe _kwForceHandlers &&
--         isResolvedMaybe _kwNotify &&
--         isResolvedMaybe _kwLoop &&
--         isResolvedMaybe _kwWhen &&
--         isResolvedMaybe _kwChangedWhen &&
--         isResolvedMaybe _kwFailedWhen &&
--         isResolvedMaybe _kwUntil &&
--         isResolvedMaybe _kwRetries
--         -- isResolvedMaybe _kwRegister

-- instance Resolvable KWLoop where
--     isResolved (KWLoop _loopList _loopVar _indexVar _pause) =
--         isResolved _loopList &&
--         isResolved _loopVar &&
--         isResolvedMaybe _indexVar &&
--         isResolvedMaybe _pause

-- instance Resolvable Var where
--     isResolved var = case var of
--         ListVar listVar -> all isResolved listVar
--         DictVar msv -> all isResolved (elems msv)
--         SimpleVarBool _ -> True
--         SimpleVarFloat _ -> True
--         SimpleVarInt _ -> True
--         SimpleVarString _ -> True
--         VarContainingJinja _ -> False

-- -- checkKWLoopAllLiteral :: KWLoop -> Bool
-- -- checkKWLoopAllLiteral kwLoop = let

-- unrollLoopAllLiteral :: Task -> Maybe [Task]
-- unrollLoopAllLiteral (AtomicTask (AttributeSet {kwLoop = Just kwLoop}) modDecl) = do
--     let allLiteral = isResolved (AtomicTask (AttributeSet {kwLoop = Just kwLoop}) modDecl)
--     -- generateTask :: Var -> Task
--     -- generateTask var = 
--     undefined