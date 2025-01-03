{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
module Semantics.Desugar.DesugarLoops where

import GrammarTypes.AnsibleGrammarTypes
import Data.Map (elems, Map, empty, insert, lookup, map, fromList, singleton)
import Control.Monad.Reader (Reader, ask, MonadTrans (lift), runReader, local)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Data.Maybe (fromJust, mapMaybe, fromMaybe, isJust, catMaybes)
import Data.List.NonEmpty (filter, intersperse, toList, NonEmpty ((:|)), fromList)
import qualified Text.Regex.TDFA.CorePattern as Data.List
import Control.Monad (zipWithM)
import Semantics.UIDSetter (buildUID)
import Semantics.StaticVarResolver (SymbolTable(SymbolTable), UVRResolvable (resolveContainedUVRs))

unrollLoopBasic :: KWLoop -> ModDecl -> [ModDecl]
unrollLoopBasic _kwLoop modDecl = let
    lpValLength = case loopList _kwLoop of
        (ListVar vars) -> length vars
        _ -> error "ERROR: lpVals must be ListVar!"
    in replicate lpValLength modDecl

resolveUnrolledModDeclWithLoopVar :: [ModDecl] -> Reader KWLoop [ModDecl]
resolveUnrolledModDeclWithLoopVar mds = do
    _kwLoop <- ask
    let lpVals = case loopList _kwLoop of
            (ListVar vars) -> vars
            _ -> error "ERROR: lpVals must be ListVar!"
    let lpVar = case loopVar _kwLoop of
            (SimpleVarString s) -> s
            _ -> error "ERROR: lpVar must be SimpleVarString!"
    let rUVR = Prelude.map (SymbolTable . Data.Map.singleton lpVar) lpVals
    mapM (\(u, m) -> return $ runReader (resolveContainedUVRs m) u) (zip rUVR mds)

resolveUnrolledModDeclWithPause :: [ModDecl] -> Reader KWLoop [ModDecl]
resolveUnrolledModDeclWithPause mds = do
    _kwLoop <- ask
    let pauseSeconds = case pause _kwLoop of
            Nothing -> error "ERROR: Called unroll loop with pause, but task does not have pause!"
            Just pauseSeconds' -> pauseSeconds'
    let pauseMod = GenericModDecl "GENERATED_pause" (Data.Map.fromList [("seconds", pauseSeconds)])
    return (Data.List.NonEmpty.toList (intersperse pauseMod (head mds :| tail mds)))

resolveUnrolledModDeclWithIndexVar :: [ModDecl] -> Reader KWLoop [ModDecl]
resolveUnrolledModDeclWithIndexVar mds = do
    _kwLoop <- ask
    let numberOfLoopVals = case loopList _kwLoop of
            (ListVar vars) -> length vars
            _ -> error "ERROR: lpVals must be ListVar!"
    let _indexVar = case indexVar _kwLoop of
            Nothing -> error "ERROR: Called unroll loop with indexvar, but task does not have indexvar!"
            Just _indexVar' -> case _indexVar' of
                SimpleVarString s -> s
                _ -> error "ERROR: _indexvar' must be SimpleVarString!"
    let rUVR = Prelude.map ((SymbolTable . Data.Map.singleton _indexVar) . SimpleVarInt) [0 .. numberOfLoopVals-1]
    mapM (\(u, m) -> return $ runReader (resolveContainedUVRs m) u) (zip rUVR mds)

createRegisterUnifierTask :: UID -> [Task] -> Task
createRegisterUnifierTask origTHUID thl = let
    regsList = mapMaybe (atomicRegister . thAtomicAttributeSet) thl
    regsAsVars = Prelude.map SimpleVarString regsList
    in Atomic (thAtomicAttributeSet $ head thl) (GenericModDecl "unify_loop_ds_regs" (Data.Map.fromList [("loop_ds_task_regs", ListVar regsAsVars)])) (buildUID origTHUID UnsetUID "unifyregs")

getReferencedRegsFromJBE :: JBE_EXP -> [JBE_REG]
getReferencedRegsFromJBE jbe = case jbe of
    JBE_EXP_REGTEST r _ _ -> [r]
    JBE_EXP_BINARYOP e1 _ e2 -> getReferencedRegsFromJBE e1 ++ getReferencedRegsFromJBE e2
    JBE_EXP_UNARYOP _ e -> getReferencedRegsFromJBE e
    JBE_EXP_PARENEXP e -> getReferencedRegsFromJBE e
    _ -> []

getReferencedTHsFromWhen :: Task -> [Task]
getReferencedTHsFromWhen (Atomic aas _ _) = let
    w = atomicWhen aas
    regs = undefined
    in undefined

unrollTH :: Task -> [Task]
unrollTH (Atomic aas modDecl uid) = let
    ml = atomicLoop aas
    in case ml of
        Nothing -> [Atomic aas modDecl uid]
        Just l -> let
            modDecls = unrollLoopBasic l modDecl
            modDecls' = runReader (resolveUnrolledModDeclWithLoopVar modDecls) l
            modDecls'' = runReader (resolveUnrolledModDeclWithIndexVar modDecls') l
            in Prelude.map (\mdcl -> Atomic aas mdcl uid) modDecls''
unrollTH (ContainingBlock bas blk uid) = let
    bm = Data.List.NonEmpty.fromList $ concatMap unrollTH $ blockMain blk
    r = fmap (Data.List.NonEmpty.fromList . concatMap unrollTH) (rescue blk)
    a = fmap (Data.List.NonEmpty.fromList . concatMap unrollTH) (always blk)
    in [ContainingBlock bas Block {blockMain=bm, rescue=r, always=a} uid]

unrollLoopsInPlay :: Play -> Play
unrollLoopsInPlay p = let
    tl = concatMap unrollTH $ tasks p
    hl = concatMap unrollTH $ handlers p
    in p {tasks=tl, handlers=hl}
