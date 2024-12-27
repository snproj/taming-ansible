{-# LANGUAGE LambdaCase #-}
module Semantics.Desugar.DesugarImports where

import GrammarTypes.AnsibleGrammarTypes
import Control.Monad.Reader (Reader, ask, local)
import Data.Map (lookup)
import Data.List.NonEmpty (filter, toList, fromList, map)
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map

getRSDFN :: String -> RoleSubDirFileName
getRSDFN s = case s of
    "main" -> MainName
    other -> OtherName other

getTasksFromRole :: Var -> Var -> Reader (RootDir, AttributeSet) (Maybe [TH TaskMarker])
getTasksFromRole (SimpleVarString roleName) (SimpleVarString taskFileName) = do
    (rd, _) <- ask
    let _roledir = roledir rd
    case _roledir of
        Nothing -> return Nothing
        Just msr -> case Data.Map.lookup roleName msr of
            Nothing -> return Nothing
            Just role -> case tasksDir role of
                Nothing -> return Nothing
                Just mnt -> case Data.Map.lookup (getRSDFN taskFileName) mnt of
                    Nothing -> return Nothing
                    Just tl -> return (Just tl)
getTasksFromRole _ _ = error ""

getHandlersFromRole :: Var -> Var -> Reader (RootDir, AttributeSet) (Maybe [TH HandlerMarker])
getHandlersFromRole (SimpleVarString roleName) (SimpleVarString handlerFileName) = do
    (rd, _) <- ask
    let _roledir = roledir rd
    case _roledir of
        Nothing -> return Nothing
        Just msr -> case Data.Map.lookup roleName msr of
            Nothing -> return Nothing
            Just role -> case handlersDir role of
                Nothing -> return Nothing
                Just mnt -> case Data.Map.lookup (getRSDFN handlerFileName) mnt of
                    Nothing -> return Nothing
                    Just hl -> return (Just hl)
getHandlersFromRole _ _ = error ""

getTasksFromLooseFile :: Var -> Reader (RootDir, AttributeSet) (Maybe [TH TaskMarker])
getTasksFromLooseFile (SimpleVarString s) = do
    (rd, _) <- ask
    case looseTaskFiles rd of
        Nothing -> return Nothing
        Just msf -> return $ Map.lookup s msf
getTasksFromLooseFile _ = error "ERROR: input to getTasksFromLooseFile must be SimpleVarString!"

taskToHandler :: TH TaskMarker -> TH HandlerMarker
taskToHandler (Atomic attSet modDecl uid) = Atomic attSet modDecl uid
taskToHandler (ContainingBlock attSet block uid) = ContainingBlock attSet (blockToHandler block) uid

blockToHandler :: Block TaskMarker -> Block HandlerMarker
blockToHandler (Block mainBlock _rescue _always) = Block (Data.List.NonEmpty.map taskToHandler mainBlock) (fmap (Data.List.NonEmpty.map taskToHandler) _rescue) (fmap (Data.List.NonEmpty.map taskToHandler) _always)

getImportsFromHandler :: TH HandlerMarker -> Reader (RootDir, AttributeSet) [TH HandlerMarker]
getImportsFromHandler (Atomic attSet modDecl uid) = case modDecl of
    (GenericModDecl _ _) -> return []
    (IncludeTasks _ _) -> return []
    (ImportRole _name _tasks_from _handlers_from) -> error ""
    (IncludeRole _ _ _ _) -> error ""
    (ImportTasks filename) -> do
        new_tasks <- getTasksFromLooseFile filename
        let new_tasks' = fromMaybe [] new_tasks
        let new_handlers = Prelude.map taskToHandler new_tasks'
        return new_handlers
getImportsFromHandler (ContainingBlock attSet block uid) = undefined -- TODO: complete

getImportsFromTask :: TH TaskMarker -> Reader (RootDir, AttributeSet) ([TH TaskMarker], [TH HandlerMarker])
getImportsFromTask (Atomic attSet modDecl uid) = case modDecl of
    (GenericModDecl _ _) -> return ([Atomic attSet modDecl uid], [])
    (IncludeTasks _ _) -> return ([Atomic attSet modDecl uid], [])
    (IncludeRole _ _ _ _) -> return ([Atomic attSet modDecl uid], [])
    (ImportRole _name _tasks_from _handlers_from) -> do
        -- TODO: apply attSet to new_tasks and new_handlers
        new_tasks <- getTasksFromRole _name _tasks_from
        let new_tasks' = fromMaybe [] new_tasks
        new_handlers <- getHandlersFromRole _name _handlers_from
        let new_handlers' = fromMaybe [] new_handlers
        return (new_tasks', new_handlers')
    (ImportTasks filename) -> do
        new_tasks <- getTasksFromLooseFile filename
        let new_tasks' = fromMaybe [] new_tasks
        return (new_tasks', [])
getImportsFromTask (ContainingBlock attSet (Block _blockMain _rescue _always) uid) = do
    (rd, attSetUpper) <- ask
    (tlbm, hlbm) <- local (const (rd, attSet)) (collateGetImportsFromTasks (toList _blockMain))
    -- let x = fmap toList _rescue
    (tlr, hlr) <- case _rescue of
        Nothing -> return ([], [])
        Just _rescue' -> do
            let rescueList = toList _rescue'
            local (const (rd, attSet)) (collateGetImportsFromTasks rescueList)
    let tlr' = if null tlr then Nothing else Just $ fromList tlr
    (tla, hla) <- case _always of
        Nothing -> return ([], [])
        Just _always' -> do
            let alwaysList = toList _always'
            local (const (rd, attSet)) (collateGetImportsFromTasks alwaysList)
    let tla' = if null tla then Nothing else Just $ fromList tla
    let newBlock = ContainingBlock attSet (Block {blockMain=fromList tlbm, rescue=tlr', always=tla'}) uid
    let hl = foldl appendHandlersWithDupRemoval [] [hlbm, hlr, hla]
    return ([newBlock], hl)

collateGetImportsFromTasks :: [TH TaskMarker] -> Reader (RootDir, AttributeSet) ([TH TaskMarker], [TH HandlerMarker])
collateGetImportsFromTasks tl = do
    tupList <- traverse getImportsFromTask tl
    let (expandedTL, handlersFromTasks) = unzip tupList
    let dupRemHandlerList = foldl appendHandlersWithDupRemoval [] handlersFromTasks
    return (concat expandedTL, dupRemHandlerList)

desugarImports :: ([TH TaskMarker], [TH HandlerMarker]) -> Reader (RootDir, AttributeSet) ([TH TaskMarker], [TH HandlerMarker])
desugarImports (tl, hl) = do
    (expandedTL, handlersFromTasks) <- collateGetImportsFromTasks tl
    let hl' = appendHandlersWithDupRemoval hl handlersFromTasks
    return (expandedTL, hl')

desugarImportsInPlay :: Play -> Reader (RootDir, AttributeSet) Play
desugarImportsInPlay play = do
    let _tasks = fromMaybe [] (tasks play)
    let _handlers = fromMaybe [] (handlers play)
    (rd, _) <- ask
    (new_tasklist, new_handlerlist) <- local (const (rd, playAttributeSet play)) (desugarImports (_tasks, _handlers))
    let new_tasklist' = case new_tasklist of
            [] -> Nothing
            x -> Just x
    let new_handlerlist' = case new_handlerlist of
            [] -> Nothing
            x -> Just x
    return Play {
        hostPattern=hostPattern play,
        playAttributeSet=playAttributeSet play,
        tasks=new_tasklist',
        handlers=new_handlerlist',
        roleNames=roleNames play
    }

appendHandlersWithDupRemoval :: [TH HandlerMarker] -> [TH HandlerMarker] -> [TH HandlerMarker]
appendHandlersWithDupRemoval handlerList newHandlers = let
    remDup = Prelude.filter (`notElem` newHandlers) handlerList
    in remDup ++ newHandlers
