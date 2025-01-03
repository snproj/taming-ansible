{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE InstanceSigs #-}
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

getTasksFromRole :: Var -> Var -> Reader RootDir (Maybe [TH TaskMarker])
getTasksFromRole (SimpleVarString roleName) (SimpleVarString taskFileName) = do
    rd <- ask
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

getHandlersFromRole :: Var -> Var -> Reader RootDir (Maybe [TH HandlerMarker])
getHandlersFromRole (SimpleVarString roleName) (SimpleVarString handlerFileName) = do
    rd <- ask
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

getTasksFromLooseFile :: Var -> Reader RootDir (Maybe [TH TaskMarker])
getTasksFromLooseFile (SimpleVarString s) = do
    rd <- ask
    case looseTaskFiles rd of
        Nothing -> return Nothing
        Just msf -> return $ Map.lookup s msf
getTasksFromLooseFile _ = error "ERROR: input to getTasksFromLooseFile must be SimpleVarString!"

taskToHandler :: TH TaskMarker -> TH HandlerMarker
taskToHandler (Atomic attSet modDecl uid) = Atomic attSet modDecl uid
taskToHandler (ContainingBlock attSet block uid) = ContainingBlock attSet (blockToHandler block) uid

blockToHandler :: Block TaskMarker -> Block HandlerMarker
blockToHandler (Block mainBlock _rescue _always) = Block (Data.List.NonEmpty.map taskToHandler mainBlock) (fmap (Data.List.NonEmpty.map taskToHandler) _rescue) (fmap (Data.List.NonEmpty.map taskToHandler) _always)

joinMaybeVarLists :: Maybe Var -> Maybe Var -> Maybe Var
joinMaybeVarLists n1 n2 = case (n1, n2) of
    (Nothing, Nothing) -> Nothing
    (Just (VarContainingJinja jjes), Nothing) -> Just (VarContainingJinja jjes)
    (Nothing, Just (VarContainingJinja jjes)) -> Just (VarContainingJinja jjes)
    (Just (ListVar l), Nothing) -> Just (ListVar l)
    (Nothing, Just (ListVar l)) -> Just (ListVar l)
    (Just (ListVar l1), Just (ListVar l2)) -> Just (ListVar (l1 ++ l2))
    _ -> error ""

-- joinWhens :: Var -> Var -> Var
-- joinWhens w1 w2 = let
--     jbew1 = wrapInJBE w1
--     jbew2 = wrapInJBE w2
--     in VarContainingJinja [JinjaBooleanExp $ JBE_EXP_BINARYOP jbew1 JBE_OP_AND jbew2]
--         where
--             wrapInJBE :: Var -> JBE_EXP
--             wrapInJBE w = case w of
--                 VarContainingJinja [UnresolvedVarRef s] -> JBE_EXP_UVR s
--                 VarContainingJinja [JinjaBooleanExp jbe] -> jbe
--                 SimpleVarBool b -> JBE_EXP_PRIM b
--                 _ -> error ""

getJBE :: Var -> JBE_EXP
getJBE (VarContainingJinja (JBEPhrase jbe)) = jbe
getJBE v = error $ show v 

joinWhens :: Var -> Var -> Var
joinWhens w1 w2 = VarContainingJinja (JBEPhrase (JBE_EXP_BINARYOP (getJBE w1) JBE_OP_AND (getJBE w2)))

applyParentAtomicAttSetToImportedTask :: AtomicAttributeSet -> TH a -> TH a
applyParentAtomicAttSetToImportedTask aas imported = case imported of
    (Atomic attSet modDecl uid) -> Atomic (attSet {
        atomicNotify = joinMaybeVarLists (atomicNotify aas) (atomicNotify attSet),
        atomicWhen = joinWhens (atomicWhen aas) (atomicWhen attSet),
        atomicVars = joinMaybeVarLists (atomicVars aas) (atomicVars attSet)
        }) modDecl uid
    (ContainingBlock attSet _block uid) -> ContainingBlock (attSet {
        blockNotify = joinMaybeVarLists (atomicNotify aas) (blockNotify attSet),
        blockWhen = joinWhens (atomicWhen aas) (blockWhen attSet),
        blockVars = joinMaybeVarLists (atomicVars aas) (blockVars attSet)
        }) _block uid

getImportsFromTask :: TH TaskMarker -> Reader RootDir ([TH TaskMarker], [TH HandlerMarker])
getImportsFromTask (Atomic attSet modDecl uid) = case modDecl of
    (GenericModDecl _ _) -> return ([Atomic attSet modDecl uid], [])
    (IncludeTasks _ _) -> return ([Atomic attSet modDecl uid], [])
    (IncludeRole _ _ _ _) -> return ([Atomic attSet modDecl uid], [])
    (ImportRole _name _tasks_from _handlers_from) -> do
        -- TODO: apply attSet to new_tasks and new_handlers
        new_tasks <- getTasksFromRole _name _tasks_from
        let new_tasks' = fromMaybe [] new_tasks
        let new_tasks'' = Prelude.map (applyParentAtomicAttSetToImportedTask attSet) new_tasks'
        new_handlers <- getHandlersFromRole _name _handlers_from
        let new_handlers' = fromMaybe [] new_handlers
        let new_handlers'' = Prelude.map (applyParentAtomicAttSetToImportedTask attSet) new_handlers'
        return (new_tasks'', new_handlers'')
    (ImportTasks filename) -> do
        new_tasks <- getTasksFromLooseFile filename
        let new_tasks' = fromMaybe [] new_tasks
        let new_tasks'' = Prelude.map (applyParentAtomicAttSetToImportedTask attSet) new_tasks'
        return (new_tasks'', [])
getImportsFromTask (ContainingBlock attSet (Block _blockMain _rescue _always) uid) = do
    (tlbm, hlbm) <- collateGetImportsFromTasks (toList _blockMain)
    (tlr, hlr) <- case _rescue of
        Nothing -> return ([], [])
        Just _rescue' -> do
            let rescueList = toList _rescue'
            collateGetImportsFromTasks rescueList
    let tlr' = if null tlr then Nothing else Just $ fromList tlr
    (tla, hla) <- case _always of
        Nothing -> return ([], [])
        Just _always' -> do
            let alwaysList = toList _always'
            collateGetImportsFromTasks alwaysList
    let tla' = if null tla then Nothing else Just $ fromList tla
    let newBlock = ContainingBlock attSet (Block {blockMain=fromList tlbm, rescue=tlr', always=tla'}) uid
    let hl = foldl appendHandlersWithDupRemoval [] [hlbm, hlr, hla]
    return ([newBlock], hl)

collateGetImportsFromTasks :: [TH TaskMarker] -> Reader RootDir ([TH TaskMarker], [TH HandlerMarker])
collateGetImportsFromTasks tl = do
    tupList <- traverse getImportsFromTask tl
    let (expandedTL, handlersFromTasks) = unzip tupList
    let dupRemHandlerList = foldl appendHandlersWithDupRemoval [] handlersFromTasks
    return (concat expandedTL, dupRemHandlerList)

desugarImports :: ([TH TaskMarker], [TH HandlerMarker]) -> Reader RootDir ([TH TaskMarker], [TH HandlerMarker])
desugarImports (tl, hl) = do
    (expandedTL, handlersFromTasks) <- collateGetImportsFromTasks tl
    let hl' = appendHandlersWithDupRemoval hl handlersFromTasks
    return (expandedTL, hl')

desugarImportsInPlay :: Play -> Reader RootDir Play
desugarImportsInPlay play = do
    let _tasks = tasks play
    let _handlers = handlers play
    (new_tasklist, new_handlerlist) <- desugarImports (_tasks, _handlers)
    return Play {
        hostPattern=hostPattern play,
        playAttributeSet=playAttributeSet play,
        tasks=new_tasklist,
        handlers=new_handlerlist,
        roleNames=roleNames play
    }

appendHandlersWithDupRemoval :: [TH HandlerMarker] -> [TH HandlerMarker] -> [TH HandlerMarker]
appendHandlersWithDupRemoval handlerList newHandlers = let
    remDup = Prelude.filter (`notElem` newHandlers) handlerList
    in remDup ++ newHandlers
