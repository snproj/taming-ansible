{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE InstanceSigs #-}
module Semantics.StaticVarResolver
    (
        SymbolTable(..),
        UVRResolvable(..),
    ) where
import Control.Monad.Reader (Reader, MonadReader (..))
import GrammarTypes.AnsibleGrammarTypes
import Data.Map (Map, lookup, union)

newtype SymbolTable = SymbolTable (Map String Var)

-- resolveJJP :: 

-- resolveJJE :: JinjaElem -> Reader SymbolTable (Either JinjaElem Var)
-- resolveJJE (UnresolvedVarRef s) = do
--     SymbolTable st <- ask
--     return (case Data.Map.lookup s st of
--         Just v -> Right v
--         Nothing -> Left (UnresolvedVarRef s))
-- resolveJJE jje = return $ Left jje

-- resolveJJEList :: [JinjaElem] -> Reader SymbolTable Var
-- resolveJJEList jjes = let
--     anyUVRs = any (\case {UnresolvedVarRef _ -> True; _ -> False}) jjes
--     anyJustStrings = any (\case {JustString _ -> True; _ -> False}) jjes
--     anyJBEs = any (\case JinjaBooleanExp _ -> True; _ -> False) jjes
--     isSingletonList = length jjes == 1
--     case (anyUVRs, anyJustStrings, anyJBEs, isSingletonList) of
--         (False, True, False, True) -> undefined
--     in undefined

resolveJSE :: JinjaStringElem -> Reader SymbolTable JinjaStringElem
resolveJSE jse = case jse of
    JSE_STRING _ -> return jse
    JSE_UVR s -> do
        SymbolTable msv <- ask
        case Data.Map.lookup s msv of
            Just v -> return $ JSE_STRING $ show v
            Nothing -> return jse

resolveJBE :: JBE_EXP -> Reader SymbolTable JBE_EXP
resolveJBE jbe = case jbe of
    JBE_EXP_BINARYOP left op right -> JBE_EXP_BINARYOP <$> resolveJBE left <*> pure op <*> resolveJBE right
    JBE_EXP_UNARYOP op term -> JBE_EXP_UNARYOP op <$> resolveJBE term
    JBE_EXP_PARENEXP term -> JBE_EXP_PARENEXP <$> resolveJBE term
    JBE_EXP_UVR s -> do
        SymbolTable msv <- ask
        case Data.Map.lookup s msv of
            Just v -> return $ JBE_EXP_UNIMPL $ show v
            Nothing -> return $ JBE_EXP_UVR s
    x -> return x

resolveJJP :: JinjaPhrase -> Reader SymbolTable Var
resolveJJP (SingletonUVR s) = do
    SymbolTable msv <- ask
    case Data.Map.lookup s msv of
        Just v -> return v
        Nothing -> return $ VarContainingJinja $ SingletonUVR s
resolveJJP (AllEventuallyString ss) = do
    resolved <- traverse resolveJSE ss
    return $ VarContainingJinja $ AllEventuallyString resolved
resolveJJP (JBEPhrase jbe) = do
    resolved <- resolveJBE jbe
    return $ VarContainingJinja $ JBEPhrase resolved

class UVRResolvable a where
    resolveContainedUVRs :: a -> Reader SymbolTable a

instance UVRResolvable Var where
    resolveContainedUVRs :: Var -> Reader SymbolTable Var
    resolveContainedUVRs (VarContainingJinja jjp) = resolveJJP jjp
    resolveContainedUVRs (DictVar msv) = do
        msv' <- traverse resolveContainedUVRs msv
        return (DictVar msv')
    resolveContainedUVRs (ListVar ls) = do
        ls' <- traverse resolveContainedUVRs ls
        return (ListVar ls')
    resolveContainedUVRs var = return var -- all SimpleVarX subtypes

instance UVRResolvable ModDecl where
    resolveContainedUVRs :: ModDecl -> Reader SymbolTable ModDecl
    resolveContainedUVRs (GenericModDecl s msv) = do
        resolvedMSV <- traverse resolveContainedUVRs msv
        return (GenericModDecl s resolvedMSV)
    resolveContainedUVRs (ImportTasks v) = do
        v' <- resolveContainedUVRs v
        return (ImportTasks v')
    resolveContainedUVRs (IncludeTasks _apply _file) = do
        _apply' <- traverse resolveContainedUVRs _apply
        _file' <- resolveContainedUVRs _file
        return (IncludeTasks _apply' _file')
    resolveContainedUVRs (ImportRole _name _tasks_from _handlers_from) = do
        _name' <- resolveContainedUVRs _name
        _tasks_from' <- resolveContainedUVRs _tasks_from
        _handlers_from' <- resolveContainedUVRs _handlers_from
        return (ImportRole _name' _tasks_from' _handlers_from')
    resolveContainedUVRs (IncludeRole _apply _name _tasks_from _handlers_from) = do
        _apply' <- traverse resolveContainedUVRs _apply
        _name' <- resolveContainedUVRs _name
        _tasks_from' <- resolveContainedUVRs _tasks_from
        _handlers_from' <- resolveContainedUVRs _handlers_from
        return (IncludeRole _apply' _name' _tasks_from' _handlers_from')

instance UVRResolvable KWLoop where
    resolveContainedUVRs :: KWLoop -> Reader SymbolTable KWLoop
    resolveContainedUVRs (KWLoop _loopList _loopVar _indexVar _pause) = do
        _loopList' <- resolveContainedUVRs _loopList
        _loopVar' <- resolveContainedUVRs _loopVar
        _indexVar' <- traverse resolveContainedUVRs _indexVar
        _pause' <- traverse resolveContainedUVRs _pause
        return (KWLoop _loopList' _loopVar' _indexVar' _pause')

instance UVRResolvable AtomicAttributeSet where
    resolveContainedUVRs :: AtomicAttributeSet -> Reader SymbolTable AtomicAttributeSet
    resolveContainedUVRs (AtomicAttributeSet
        _atomicNotify
        _atomicLoop
        _atomicWhen
        _atomicVars
        _atomicChangedWhen
        _atomicFailedWhen
        _atomicUntil
        _atomicRetries
        _atomicRegister
        _atomicIgnoreErrors
        _atomicListen
        -- _
        ) = do
            _atomicNotify' <- traverse resolveContainedUVRs _atomicNotify
            _atomicLoop' <- traverse resolveContainedUVRs _atomicLoop
            _atomicWhen' <- resolveContainedUVRs _atomicWhen
            _atomicVars' <- traverse resolveContainedUVRs _atomicVars
            _atomicChangedWhen' <- traverse resolveContainedUVRs _atomicChangedWhen
            _atomicFailedWhen' <- traverse resolveContainedUVRs _atomicFailedWhen
            _atomicUntil' <- traverse resolveContainedUVRs _atomicUntil
            _atomicRetries' <- resolveContainedUVRs _atomicRetries
            _atomicIgnoreErrors' <- resolveContainedUVRs _atomicIgnoreErrors
            return (AtomicAttributeSet 
                _atomicNotify'
                _atomicLoop'
                _atomicWhen'
                _atomicVars'
                _atomicChangedWhen'
                _atomicFailedWhen'
                _atomicUntil'
                _atomicRetries'
                _atomicRegister
                _atomicIgnoreErrors'
                _atomicListen
                -- Nothing
                )

instance UVRResolvable BlockAttributeSet where
    resolveContainedUVRs :: BlockAttributeSet -> Reader SymbolTable BlockAttributeSet
    resolveContainedUVRs (BlockAttributeSet
        _blockNotify
        _blockWhen
        _blockVars
        -- _
        ) = do
            _blockNotify' <- traverse resolveContainedUVRs _blockNotify
            _blockWhen' <- resolveContainedUVRs _blockWhen
            _blockVars' <- traverse resolveContainedUVRs _blockVars
            return (BlockAttributeSet
                _blockNotify'
                _blockWhen'
                _blockVars'
                -- Nothing
                )

instance UVRResolvable PlayAttributeSet where
    resolveContainedUVRs :: PlayAttributeSet -> Reader SymbolTable PlayAttributeSet
    resolveContainedUVRs (PlayAttributeSet
        _playVars
        ) = do
            _playVars' <- traverse resolveContainedUVRs _playVars
            return (PlayAttributeSet
                _playVars'
                )

instance UVRResolvable Block where
    resolveContainedUVRs :: Block -> Reader SymbolTable Block
    resolveContainedUVRs (Block _blockMain _rescue _always) = do
        _blockMain' <- traverse resolveContainedUVRs _blockMain
        _rescue' <- traverse (traverse resolveContainedUVRs) _rescue
        _always' <- traverse (traverse resolveContainedUVRs) _always
        return (Block _blockMain' _rescue' _always')

instance UVRResolvable Task where
    resolveContainedUVRs :: Task -> Reader SymbolTable Task
    resolveContainedUVRs (Atomic attSet modDecl uid) = do
        attSet' <- resolveContainedUVRs attSet
        let newScopeAddons = atomicVars attSet'
        modDecl' <- maybeWithNewScope newScopeAddons resolveContainedUVRs modDecl
        return (Atomic attSet' modDecl' uid)
    resolveContainedUVRs (ContainingBlock attSet blockTask uid) = do
        attSet' <- resolveContainedUVRs attSet
        let newScopeAddons = blockVars attSet'
        blockTask' <- maybeWithNewScope newScopeAddons resolveContainedUVRs blockTask
        return (ContainingBlock attSet' blockTask' uid)

instance UVRResolvable Play where
    resolveContainedUVRs :: Play -> Reader SymbolTable Play
    resolveContainedUVRs (Play
        _hostPattern
        _attributeSet
        _tasks
        _handlers
        _roleNames) = do
            _attributeSet' <- resolveContainedUVRs _attributeSet
            let newScopeAddons = playVars _attributeSet'
            _tasks' <- traverse (maybeWithNewScope newScopeAddons resolveContainedUVRs) _tasks
            _handlers' <- traverse (maybeWithNewScope newScopeAddons resolveContainedUVRs) _handlers
            return (Play _hostPattern _attributeSet' _tasks' _handlers' _roleNames)

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







