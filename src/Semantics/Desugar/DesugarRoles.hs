{-# LANGUAGE LambdaCase #-}
module Semantics.Desugar.DesugarRoles where

import GrammarTypes.AnsibleGrammarTypes
import Data.Map (elems, Map, empty, insert, lookup, map)
import Control.Monad.Reader (Reader, ask, MonadTrans (lift), runReader)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Data.Maybe (fromJust, mapMaybe, fromMaybe, isJust)
import Data.List.NonEmpty (filter)
import qualified Data.Map as Map
import Semantics.UIDSetter (setUID)


assignImportUIDs :: String -> [Task] -> [Task]
assignImportUIDs s ts = runReader (setUID ts) (SetUID s)

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
            let newTs = flattenedTaskLists ++ ts
            -- sad code duplication for tasks and handlers
            let handlerLists = mapMaybe getMainHandlersFromRole jRoles
            -- let flattenedHandlerLists = concat handlerLists
            let flattenedHandlerLists' = zipWith (\rn hl -> (assignImportUIDs (rn ++ "main") hl)) nList handlerLists
            let flattenedHandlerLists'' = concat flattenedHandlerLists'
            let newHs = flattenedHandlerLists'' ++ hs

            return (Play hp attSet newTs newHs Nothing)
    return (fromMaybe (Play hp attSet ts hs Nothing) res)


getMainTasksFromRole :: Role -> Maybe [Task]
getMainTasksFromRole r = do
    _tasksDir <- tasksDir r
    Map.lookup MainName _tasksDir

getMainHandlersFromRole :: Role -> Maybe [Task]
getMainHandlersFromRole r = do
    _handlersDir <- handlersDir r
    Map.lookup MainName _handlersDir

