{-# LANGUAGE LambdaCase #-}
module Semantics.Desugar.DesugarImports where

import GrammarTypes.AnsibleGrammarTypes
import Control.Monad.Reader (Reader, ask, local)
import Data.Map (lookup)
import Data.List.NonEmpty (filter, toList, fromList)
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
    -- case mmsr of
    --     Nothing -> return Nothing
    --     Just msr -> case Data.Map.lookup roleName msr of
    --         Nothing -> return Nothing
    --         Just (Role necrd) -> let
    --             onlyTasksDirs = Data.List.NonEmpty.filter (\case {TasksDir _ -> True; _ -> False}) necrd
    --             (TasksDir mnt) = head onlyTasksDirs -- should really only have one task dir lol
    --             in case Data.Map.lookup (getRSDFN taskFileName) mnt of
    --                 Nothing -> return Nothing
    --                 Just tl -> return (Just tl)
                -- in undefined
    -- undefined
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

-- getHandlersFromRole :: Var -> Var -> Reader RootDir (Maybe [TH HandlerMarker])
-- getHandlersFromRole (SimpleVarString roleName) (SimpleVarString handlerFileName) = do
--     RootDir _ mmsr <- ask
--     case mmsr of
--         Nothing -> return Nothing
--         Just msr -> case Data.Map.lookup roleName msr of
--             Nothing -> return Nothing
--             Just (Role necrd) ->
--                 let onlyHandlersDirs = Data.List.NonEmpty.filter (\case HandlersDir _ -> True; _ -> False) necrd
--                     (HandlersDir mnt) = head onlyHandlersDirs -- should really only have one handler dir lol
--                 in case Data.Map.lookup (getRSDFN handlerFileName) mnt of
--                     Nothing -> return Nothing
--                     Just hl -> return (Just hl)
-- -- in undefined
-- -- undefined
-- getHandlersFromRole _ _ = error ""


-- getTasksFromFile :: Var -> Reader RootDir (Maybe [TH TaskMarker])
-- getTasksFromFile (SimpleVarString filename) = do

getTasksFromLooseFile :: Var -> Reader (RootDir, AttributeSet) (Maybe [TH TaskMarker])
getTasksFromLooseFile (SimpleVarString s) = do
    (rd, _) <- ask
    case looseTaskFiles rd of
        Nothing -> return Nothing
        Just msf -> return $ Map.lookup s msf
getTasksFromLooseFile _ = error "ERROR: input to getTasksFromLooseFile must be SimpleVarString!"

getImportsFromTask :: TH TaskMarker -> Reader (RootDir, AttributeSet) ([TH TaskMarker], [TH HandlerMarker])
getImportsFromTask (Atomic attSet modDecl) = case modDecl of
    (GenericModDecl _ _) -> return ([Atomic attSet modDecl], [])
    (IncludeTasks _ _) -> return ([Atomic attSet modDecl], [])
    (IncludeRole _ _ _ _) -> return ([Atomic attSet modDecl], [])
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
-- getImportsFromTask (ContainingBlock attSet (Block neTH mRes)) = do
--     (tl, hl) <- collateGetImportsFromTasks (toList neTH)
--     (newRescue, rescueHandlers) <- case mRes of
--         Nothing -> return (Nothing, [])
--         Just rescue -> do
--             (rd, attSet2) <- ask
--             (newRescue', rescueHandlers') <- local (const (rd, attSet2)) (getImportsFromRescue rescue)
--             return (Just newRescue', rescueHandlers')
--     let resBlockTask = ContainingBlock attSet (Block (fromList tl) newRescue)
--     let hl' = appendHandlersWithDupRemoval hl rescueHandlers
--     return ([resBlockTask], hl')
getImportsFromTask (ContainingBlock attSet (Block _blockMain _rescue _always)) = do
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
    let newBlock = ContainingBlock attSet (Block {blockMain=fromList tlbm, rescue=tlr', always=tla'})
    let hl = foldl appendHandlersWithDupRemoval [] [hlbm, hlr, hla]
    return ([newBlock], hl)

-- getImportsFromRescue :: Rescue TaskMarker -> Reader (RootDir, AttributeSet) (Rescue TaskMarker, [TH HandlerMarker])
-- getImportsFromRescue (Rescue neTH mAlways) = do
--     (tl, hl) <- collateGetImportsFromTasks (toList neTH)
--     (newAlways, alwaysHandlers) <- case mAlways of
--         Nothing -> return (Nothing, [])
--         Just always -> do
--             (rd, attSet2) <- ask
--             (newAlways', alwaysHandlers') <- local (const (rd, attSet2)) (getImportsFromAlways always)
--             return (Just newAlways', alwaysHandlers')
--     let newRescue = Rescue (fromList tl) newAlways
--     let hl' = appendHandlersWithDupRemoval hl alwaysHandlers
--     return (newRescue, hl')

-- getImportsFromAlways :: Always TaskMarker -> Reader (RootDir, AttributeSet) (Always TaskMarker, [TH HandlerMarker])
-- getImportsFromAlways (Always neTH) = do
--     (tl, hl) <- collateGetImportsFromTasks (toList neTH)
--     let newAlways = Always (fromList tl)
--     return (newAlways, hl)

-- desugarImportsInBlock :: Block a -> Reader RootDir (Block a, [TH HandlerMarker])
-- desugarImportsInBlock (Block neTH mRescue) = let
--     thl = toList neTH
--     gotImports = map get
--     in undefined

-- Must be run assuming no blocks in 
-- desugarImports :: ([TH TaskMarker], [TH HandlerMarker]) -> Reader (RootDir, AttributeSet) ([TH TaskMarker], [TH HandlerMarker])
-- desugarImports (tl, hl) = crawler 0 tl hl
--     where
--         crawler :: Int -> [TH TaskMarker] -> [TH HandlerMarker] -> Reader (RootDir, AttributeSet) ([TH TaskMarker], [TH HandlerMarker])
--         crawler idx tl hl = if idx > length tl
--             then return (tl, hl)
--             else do
--                 let currElem = tl !! idx
--                 (tasksToAdd, handlersToAdd) <- case currElem of
--                     (ContainingBlock _ _) -> undefined
--                     (Atomic attSet modDecl) -> case modDecl of
--                         (ImportRole _name _tasks_from _handlers_from) -> do
--                             -- TODO: apply attSet to new_tasks and new_handlers
--                             new_tasks <- getTasksFromRole _name _tasks_from
--                             let new_tasks' = fromMaybe [] new_tasks
--                             new_handlers <- getHandlersFromRole _name _handlers_from
--                             let new_handlers' = fromMaybe [] new_handlers
--                             return (new_tasks', new_handlers')

--                         _ -> undefined
--                 undefined

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
    (new_tasklist, new_handlerlist) <- local (const (rd, attributeSet play)) (desugarImports (_tasks, _handlers))
    let new_tasklist' = case new_tasklist of
            [] -> Nothing
            x -> Just x
    let new_handlerlist' = case new_handlerlist of
            [] -> Nothing
            x -> Just x
    return Play {
        hostPattern=hostPattern play,
        attributeSet=attributeSet play,
        tasks=new_tasklist',
        handlers=new_handlerlist',
        roleNames=roleNames play
    }

appendHandlersWithDupRemoval :: [TH HandlerMarker] -> [TH HandlerMarker] -> [TH HandlerMarker]
appendHandlersWithDupRemoval handlerList newHandlers = let
    remDup = Prelude.filter (`notElem` newHandlers) handlerList
    in remDup ++ newHandlers

-- concatHandlersWithDupRemoval :: [TH HandlerMarker] -> [TH HandlerMarker] -> [TH HandlerMarker]
-- concatHandlersWithDupRemoval handlers handlerList = Prelude.map (\h -> appendHandlerWithDupRemoval h handlerList) handlers
