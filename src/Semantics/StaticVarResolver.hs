{-# LANGUAGE LambdaCase #-}
module Semantics.StaticVarResolver where
import Control.Monad.Reader (Reader, MonadReader (..))
import GrammarTypes.AnsibleGrammarTypes
import Data.Map (Map, lookup, union)

newtype SymbolTable = SymbolTable (Map String Var)

-- resolveJJUVR :: JinjaElem -> Reader SymbolTable JinjaElem
-- resolveJJUVR (UnresolvedVarRef s) = do
--     st <- ask
--     case Data.Map.lookup s st of
--         Just x -> return x
--         Nothing -> return 
-- resolveJJUVR _ = undefined

-- getVarRefString :: JinjaElem -> Maybe String
-- getVarRefString (UnresolvedVarRef s) = Just s
-- getVarRefString _ = Nothing

resolveUVR :: JinjaElem -> Reader SymbolTable (Maybe Var)
resolveUVR (UnresolvedVarRef s) = do
    SymbolTable st <- ask
    return (Data.Map.lookup s st)
resolveUVR _ = return Nothing

resolveVar :: Var -> Reader SymbolTable Var
resolveVar (VarContainingJinja jjes) = do
    uvrs <- mapM resolveUVR jjes
    let res = eitherAnySingleVarOrCombinedStringVar uvrs jjes
    return res
    where
        joinOnlyStrings :: [Var] -> Var
        joinOnlyStrings [] = error "ERROR: Empty list in string containing JJE? This should not be possible!"
        joinOnlyStrings (v : vs) = go "" (v : vs)
            where
                go :: String -> [Var] -> Var
                go s [SimpleVarString s'] = SimpleVarString (s ++ s')
                go s (SimpleVarString s' : vs') = go (s ++ s') vs'
                go _ _ = error "ERROR: Encountered unexpected variable in multi-JJE JJEVar! Should all be strings!"
        combineJustStringsAndResolvedUVRs :: Maybe Var -> JinjaElem -> Var
        combineJustStringsAndResolvedUVRs mvar jje = case (mvar, jje) of
            (Just resolved, UnresolvedVarRef _) -> resolved
            (Nothing, JustString s) -> SimpleVarString s
            (Nothing, UnresolvedVarRef s) -> VarContainingJinja [UnresolvedVarRef s]
            _ -> error "ERROR: Unexpected resolution encountered!"
        eitherAnySingleVarOrCombinedStringVar :: [Maybe Var] -> [JinjaElem] -> Var
        eitherAnySingleVarOrCombinedStringVar mvars jjes' = let
            zipped = zipWith combineJustStringsAndResolvedUVRs mvars jjes'
            in case zipped of
                [single] -> single -- can be any Var subtype; this is for constructs like `loop: "{{var1}}"`
                (v:vs) -> joinOnlyStrings (v:vs) -- must give SimpleVarString! this is for constructs like `msg: "hi {{var1}}"`
                [] -> error "ERROR: empty zip result???"
resolveVar (DictVar msv) = do
    msv' <- traverse resolveVar msv
    return (DictVar msv')
resolveVar (ListVar ls) = do
    ls' <- traverse resolveVar ls
    return (ListVar ls')
resolveVar var = return var -- all SimpleVarX subtypes

resolveModDecl :: ModDecl -> Reader SymbolTable ModDecl
resolveModDecl (GenericModDecl s msv) = do
    resolvedMSV <- traverse resolveVar msv
    return (GenericModDecl s resolvedMSV)
resolveModDecl (ImportTasks v) = do
    v' <- resolveVar v
    return (ImportTasks v')
resolveModDecl (IncludeTasks _apply _file) = do
    _apply' <- traverse resolveVar _apply
    _file' <- resolveVar _file
    return (IncludeTasks _apply' _file')
resolveModDecl (ImportRole _name _tasks_from _handlers_from) = do
    _name' <- resolveVar _name
    _tasks_from' <- resolveVar _tasks_from
    _handlers_from' <- resolveVar _handlers_from
    return (ImportRole _name' _tasks_from' _handlers_from')
resolveModDecl (IncludeRole _apply _name _tasks_from _handlers_from) = do
    _apply' <- traverse resolveVar _apply
    _name' <- resolveVar _name
    _tasks_from' <- resolveVar _tasks_from
    _handlers_from' <- resolveVar _handlers_from
    return (IncludeRole _apply' _name' _tasks_from' _handlers_from')

resolveKWLoop :: KWLoop -> Reader SymbolTable KWLoop
resolveKWLoop (KWLoop _loopList _loopVar _indexVar _pause) = do
    _loopList' <- resolveVar _loopList
    _loopVar' <- resolveVar _loopVar
    _indexVar' <- traverse resolveVar _indexVar
    _pause' <- traverse resolveVar _pause
    return (KWLoop _loopList' _loopVar' _indexVar' _pause')

resolveAttributeSet :: AttributeSet -> Reader SymbolTable AttributeSet
resolveAttributeSet (AttributeSet
    _kwName
    _kwForceHandlers
    _kwNotify
    _kwLoop
    _kwWhen
    _kwVars
    _kwChangedWhen
    _kwFailedWhen
    _kwUntil
    _kwRetries
    _kwRegister
    _kwIgnoreErrors
    _kwListen) = do
        _kwForceHandlers' <- traverse resolveVar _kwForceHandlers
        _kwNotify' <- traverse resolveVar _kwNotify
        _kwLoop' <- traverse resolveKWLoop _kwLoop
        _kwWhen' <- resolveVar _kwWhen
        _kwVars' <- traverse resolveVar _kwVars
        _kwChangedWhen' <- traverse resolveVar _kwChangedWhen
        _kwFailedWhen' <- traverse resolveVar _kwFailedWhen
        _kwUntil' <- traverse resolveVar _kwUntil
        _kwRetries' <- traverse resolveVar _kwRetries
        _kwIgnoreErrors' <- resolveVar _kwIgnoreErrors
        return (AttributeSet 
            _kwName
            _kwForceHandlers'
            _kwNotify'
            _kwLoop'
            _kwWhen'
            _kwVars'
            _kwChangedWhen'
            _kwFailedWhen'
            _kwUntil'
            _kwRetries'
            _kwRegister
            _kwIgnoreErrors'
            _kwListen
            )

-- resolveAlwaysHandler :: AlwaysHandler -> Reader SymbolTable AlwaysHandler
-- resolveAlwaysHandler (AlwaysHandler neHandler) = do
--     neHandler' <- traverse resolveHandler neHandler
--     return (AlwaysHandler neHandler')

-- resolveRescueHandler :: RescueHandler -> Reader SymbolTable RescueHandler
-- resolveRescueHandler (RescueHandler neHandler mAlwaysHandler) = do
--     neHandler' <- traverse resolveHandler neHandler
--     mAlwaysHandler' <- traverse resolveAlwaysHandler mAlwaysHandler
--     return (RescueHandler neHandler' mAlwaysHandler')

-- resolveBlockHandler :: BlockHandler -> Reader SymbolTable BlockHandler
-- resolveBlockHandler (BlockHandler neHandler mRescueHandler) = do
--     neHandler' <- traverse resolveHandler neHandler
--     mRescueHandler' <- traverse resolveRescueHandler mRescueHandler
--     return (BlockHandler neHandler' mRescueHandler')

-- resolveAlwaysTask :: Always a -> Reader SymbolTable (Always a)
-- resolveAlwaysTask (Always neTask) = do
--     neTask' <- traverse resolveTH neTask
--     return (Always neTask')

-- resolveRescueTask :: Rescue a -> Reader SymbolTable (Rescue a)
-- resolveRescueTask (Rescue neTask mAlwaysTask) = do
--     neTask' <- traverse resolveTH neTask
--     mAlwaysTask' <- traverse resolveAlwaysTask mAlwaysTask
--     return (Rescue neTask' mAlwaysTask')

-- resolveBlockTask :: Block a -> Reader SymbolTable (Block a)
-- resolveBlockTask (Block neTask mRescueTask) = do
--     neTask' <- traverse resolveTH neTask
--     mRescueTask' <- traverse resolveRescueTask mRescueTask
--     return (Block neTask' mRescueTask')

resolveBlockTask :: Block a -> Reader SymbolTable (Block a)
resolveBlockTask (Block _blockMain _rescue _always) = do
    _blockMain' <- traverse resolveTH _blockMain
    _rescue' <- traverse (traverse resolveTH) _rescue
    _always' <- traverse (traverse resolveTH) _always
    return (Block _blockMain' _rescue' _always')

resolveTH :: TH a -> Reader SymbolTable (TH a)
resolveTH (Atomic attSet modDecl uid) = do
    attSet' <- resolveAttributeSet attSet
    let newScopeAddons = kwVars attSet'
    modDecl' <- maybeWithNewScope newScopeAddons resolveModDecl modDecl
    return (Atomic attSet' modDecl' uid)
resolveTH (ContainingBlock attSet blockTask uid) = do
    attSet' <- resolveAttributeSet attSet
    let newScopeAddons = kwVars attSet'
    blockTask' <- maybeWithNewScope newScopeAddons resolveBlockTask blockTask
    return (ContainingBlock attSet' blockTask' uid)

-- resolveHandler :: Handler -> Reader SymbolTable Handler
-- resolveHandler (AtomicHandler attSet modDecl) = do
--     attSet' <- resolveAttributeSet attSet
--     let newScopeAddons = kwVars attSet'
--     modDecl' <- maybeWithNewScope newScopeAddons resolveModDecl modDecl
--     return (AtomicHandler attSet' modDecl')
-- resolveHandler (HandlerContainingABlock attSet blockHandler) = do
--     attSet' <- resolveAttributeSet attSet
--     let newScopeAddons = kwVars attSet'
--     blockHandler' <- maybeWithNewScope newScopeAddons resolveBlockHandler blockHandler
--     return (HandlerContainingABlock attSet' blockHandler')


resolvePlay :: Play -> Reader SymbolTable Play
resolvePlay (Play
    _hostPattern
    _attributeSet
    _tasks
    _handlers
    _roleNames) = do
        _attributeSet' <- resolveAttributeSet _attributeSet
        let newScopeAddons = kwVars _attributeSet'
        _tasks' <- traverse (traverse (maybeWithNewScope newScopeAddons resolveTH)) _tasks
        _handlers' <- traverse (traverse (maybeWithNewScope newScopeAddons resolveTH)) _handlers
        return (Play _hostPattern _attributeSet' _tasks' _handlers' _roleNames)

resolvePlaybook :: Playbook -> Reader SymbolTable Playbook
resolvePlaybook (PlaybookDefinedHere nePlay) = do
    nePlay' <- traverse resolvePlay nePlay
    return (PlaybookDefinedHere nePlay')

-- resolveCompulsoryRoleDir :: CompulsoryRoleDir -> Reader SymbolTable CompulsoryRoleDir
-- resolveCompulsoryRoleDir (TasksDir mnt) = do
    

extendSymbolTable :: Var -> SymbolTable -> SymbolTable
extendSymbolTable (DictVar msv) (SymbolTable st) = SymbolTable (Data.Map.union msv st)
extendSymbolTable _ _ = error "ERROR: Tried to extend symbol table with non-dict var! Can only use a DictVar!"

-- withNewScope :: Var -> Reader SymbolTable a -> Reader SymbolTable a
-- withNewScope vars = local (extendSymbolTable vars)

withNewScope :: Var -> (a -> Reader SymbolTable a) -> a -> Reader SymbolTable a
withNewScope vars resolver resolvee = do
    st <- ask
    let extendedST = extendSymbolTable vars st
    local (const extendedST) (resolver resolvee)

maybeWithNewScope :: Maybe Var -> (a -> Reader SymbolTable a) -> a -> Reader SymbolTable a
maybeWithNewScope mvars resolver resolvee = case mvars of
    Just vars -> withNewScope vars resolver resolvee
    Nothing -> resolver resolvee







