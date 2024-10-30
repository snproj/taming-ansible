{-# LANGUAGE LambdaCase #-}
module Semantics.Desugar where

import GrammarTypes.AnsibleGrammarTypes
import Data.Map (elems, Map, empty, insert, lookup, map)
import Control.Monad.Reader (Reader, ask, MonadTrans (lift))
import Control.Monad.Trans.Maybe (MaybeT(..))
import Data.Maybe (fromJust, mapMaybe, fromMaybe, isJust)
import Data.List.NonEmpty (filter)

-- desugarPlainRoleCalls :: Play -> Reader (Maybe (Map String Role)) Play
-- desugarPlainRoleCalls (Play hp attSet ts hs rNames) = do
--     roles <- ask
--     case roles of
--         Nothing -> return (Play hp attSet ts hs rNames)
--         Just msr -> case rNames of
--             Nothing -> return (Play hp attSet ts hs rNames)
--             Just nList -> do 


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





getMainTasksFromRole :: Role -> Maybe [Task]
getMainTasksFromRole (Role neCRD) = do
    let justTaskDirs = Data.List.NonEmpty.filter (\case {TasksDir _ -> True; _ -> False}) neCRD
    taskDir <- case justTaskDirs of
        [td] -> Just td
        [] -> Nothing
        _ -> error ""
    getTaskFileFromTaskDir taskDir MainName


getTaskFileFromTaskDir :: CompulsoryRoleDir -> RoleSubDirFileName -> Maybe [Task]
getTaskFileFromTaskDir (TasksDir mnt) n = Data.Map.lookup n mnt
getTaskFileFromTaskDir _ _ = error "ERROR: Tried to get task file from non-task dir"

getMainHandlersFromRole :: Role -> Maybe [Handler]
getMainHandlersFromRole (Role neCRD) = do
  let justHandlerDirs = Data.List.NonEmpty.filter (\case HandlersDir _ -> True; _ -> False) neCRD
  handlerDir <- case justHandlerDirs of
    [td] -> Just td
    [] -> Nothing
    _ -> error ""
  getHandlerFileFromHandlerDir handlerDir MainName

getHandlerFileFromHandlerDir :: CompulsoryRoleDir -> RoleSubDirFileName -> Maybe [Handler]
getHandlerFileFromHandlerDir (HandlersDir mnt) n = Data.Map.lookup n mnt
getHandlerFileFromHandlerDir _ _ = error "ERROR: Tried to get handler file from non-handler dir"

insertLoopVarString :: String -> String -> [JinjaElem] -> [JinjaElem]
-- insertLoopVarString "item" "1"
insertLoopVarString lpVar lpVal = Prelude.map (\case
        JustString t -> JustString t
        UnresolvedVarRef n -> if lpVar == n
            then JustString lpVal
            else UnresolvedVarRef n)

jjeJustStringToString :: JinjaElem -> Maybe String
jjeJustStringToString jje = case jje of
    JustString s -> Just s
    UnresolvedVarRef _ -> Nothing

jjeAllResolvedToString :: [JinjaElem] -> Either [JinjaElem] String
jjeAllResolvedToString jjes = let
    mStrings = Prelude.map jjeJustStringToString jjes
    in if all isJust mStrings
        then Right $ concatMap fromJust mStrings
        else Left jjes

insertLoopVar :: String -> Var -> Var -> Var
-- insertLoopVar "item" "1" "hi {{item}}"
insertLoopVar lpVar lpVal jjeVar = case jjeVar of
    VarContainingJinja jjeVar' -> let
        lpVal' = show lpVal
        inserted = insertLoopVarString lpVar lpVal' jjeVar'
        in case jjeAllResolvedToString inserted of
            Left jjes -> VarContainingJinja jjes
            Right s -> SimpleVarString s
    ListVar varList -> let
        varList' = Prelude.map (insertLoopVar lpVar lpVal) varList
        in ListVar varList'
    DictVar msv -> let
        msv' = Data.Map.map (insertLoopVar lpVar lpVal) msv
        in DictVar msv'
    SimpleVarBool b -> SimpleVarBool b
    SimpleVarFloat f -> SimpleVarFloat f
    SimpleVarInt i -> SimpleVarInt i
    SimpleVarString s -> SimpleVarString s



insertOneLoopVar :: String -> Map String Var -> Var -> Map String Var
-- insertOneLoopVar "item" somemap "1"
insertOneLoopVar lpVar msv lpVal = Data.Map.map (insertLoopVar lpVar lpVal) msv

unrollLoop :: KWLoop -> ModDecl -> [ModDecl]
unrollLoop _kwLoop modDecl = let
    lpVals = case loopList _kwLoop of
        (ListVar vars) -> vars
        _ -> error "ERROR: lpVal must be ListVar!"
    lpVar = case loopVar _kwLoop of
        (SimpleVarString s) -> s
        _ -> error "lpVar must be SimpleVarString!"
    in case modDecl of
        (GenericModDecl _name msv) -> let
            msvs = Prelude.map (insertOneLoopVar lpVar msv) lpVals
            modDecls = Prelude.map (GenericModDecl _name) msvs
            in modDecls
        _ -> error "ERROR: Loop unrolling not implemented yet for non-generic modDecls!"




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