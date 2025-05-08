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




class UVRResolvable a where
    resolveContainedUVRs :: a -> Reader SymbolTable a









instance UVRResolvable Play where
    resolveContainedUVRs :: Play -> Reader SymbolTable Play
    resolveContainedUVRs (Play
        _vars
        _tasks
        _handlers
        _roleNames) = do
            -- let newScopeAddons = playVars _attributeSet'
            _tasks' <- traverse (withNewScope _vars resolveContainedUVRs) _tasks
            _handlers' <- traverse (withNewScope _vars resolveContainedUVRs) _handlers
            return (Play _vars _tasks' _handlers' _roleNames)


instance UVRResolvable Task where
    resolveContainedUVRs :: Task -> Reader SymbolTable Task
    resolveContainedUVRs (Atomic attSet md uid) = do
        attSet' <- resolveContainedUVRs attSet
        let newScopeAddons = atomicVars attSet'
        md' <- withNewScope newScopeAddons resolveContainedUVRs md
        return (Atomic attSet' md' uid)
    resolveContainedUVRs (Blocktask attSet blockTask uid) = do
        attSet' <- resolveContainedUVRs attSet
        let newScopeAddons = blockVars attSet'
        blockTask' <- withNewScope newScopeAddons resolveContainedUVRs blockTask
        return (Blocktask attSet' blockTask' uid)


instance UVRResolvable AtomicAttributeSet where
    resolveContainedUVRs :: AtomicAttributeSet -> Reader SymbolTable AtomicAttributeSet
    resolveContainedUVRs (AtomicAttributeSet
        _atomicNotify
        _atomicLoop
        _atomicListen
        _atomicVars
        _atomicWhen
        _atomicIgnoreErrors
        -- _
        ) = do
            _atomicNotify' <- traverse resolveContainedUVRs _atomicNotify
            _atomicLoop' <- traverse resolveContainedUVRs _atomicLoop
            _atomicListen' <- traverse resolveContainedUVRs _atomicListen
            _atomicVars' <- traverse resolveContainedUVRs _atomicVars
            return (AtomicAttributeSet 
                _atomicNotify'
                _atomicLoop'
                _atomicListen'
                _atomicVars'
                _atomicWhen
                _atomicIgnoreErrors
                -- Nothing
                )


instance UVRResolvable KWLoop where
    resolveContainedUVRs :: KWLoop -> Reader SymbolTable KWLoop
    resolveContainedUVRs (KWLoop _loopList _loopVar _indexVar) = do
        _loopList' <- traverse resolveContainedUVRs _loopList
        _loopVar' <- resolveContainedUVRs _loopVar
        _indexVar' <- traverse resolveContainedUVRs _indexVar
        return (KWLoop _loopList' _loopVar' _indexVar')

instance UVRResolvable BlockAttributeSet where
    resolveContainedUVRs :: BlockAttributeSet -> Reader SymbolTable BlockAttributeSet
    resolveContainedUVRs (BlockAttributeSet
        _blockNotify
        _blockVars
        -- _
        ) = do
            _blockNotify' <- traverse resolveContainedUVRs _blockNotify
            _blockVars' <- traverse resolveContainedUVRs _blockVars
            return (BlockAttributeSet
                _blockNotify'
                _blockVars'
                )



instance UVRResolvable Block where
    resolveContainedUVRs :: Block -> Reader SymbolTable Block
    resolveContainedUVRs (Block _blockMain _rescue _always _goalkeeper) = do
        _blockMain' <- traverse resolveContainedUVRs _blockMain
        _rescue' <- traverse resolveContainedUVRs _rescue
        _always' <- traverse resolveContainedUVRs _always
        return (Block _blockMain' _rescue' _always' _goalkeeper)



instance UVRResolvable ModDecl where
    resolveContainedUVRs :: ModDecl -> Reader SymbolTable ModDecl
    resolveContainedUVRs (GenericModDecl s msv) = do
        resolvedMSV <- traverse resolveContainedUVRs msv
        return (GenericModDecl s resolvedMSV)
    resolveContainedUVRs (ImportTasks v) = do
        v' <- resolveContainedUVRs v
        return (ImportTasks v')
    resolveContainedUVRs (ImportRole _name _tasks_from _handlers_from) = do
        _name' <- resolveContainedUVRs _name
        _tasks_from' <- resolveContainedUVRs _tasks_from
        _handlers_from' <- resolveContainedUVRs _handlers_from
        return (ImportRole _name' _tasks_from' _handlers_from')






instance UVRResolvable Var where
    resolveContainedUVRs :: Var -> Reader SymbolTable Var
    resolveContainedUVRs (VarContainingJinja jjp) = resolveJJP jjp
    resolveContainedUVRs var = return var -- all SimpleVarX subtypes






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
    JBE_EXP_NOT term -> JBE_EXP_NOT <$> resolveJBE term
    JBE_EXP_PARENEXP term -> JBE_EXP_PARENEXP <$> resolveJBE term
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







extendSymbolTable :: Map String Var -> SymbolTable -> SymbolTable
extendSymbolTable msv (SymbolTable st) = SymbolTable (Data.Map.union msv st)
extendSymbolTable _ _ = error "ERROR: Tried to extend symbol table with non-dict var! Can only use a DictVar!"

withNewScope :: Map String Var -> (a -> Reader SymbolTable a) -> a -> Reader SymbolTable a
withNewScope v resolver resolvee = do
    st <- ask
    let extendedST = extendSymbolTable v st
    local (const extendedST) (resolver resolvee)






