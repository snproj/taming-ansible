{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
module Semantics.Desugar.DesugarLoops where

import GrammarTypes.AnsibleGrammarTypes
import Data.Map (elems, Map, empty, insert, lookup, map, fromList)
import Control.Monad.Reader (Reader, ask, MonadTrans (lift), runReader, local)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Data.Maybe (fromJust, mapMaybe, fromMaybe, isJust)
import Data.List.NonEmpty (filter, intersperse, toList, NonEmpty ((:|)))
import qualified Text.Regex.TDFA.CorePattern as Data.List
import Control.Monad (zipWithM)

data UVRResolve = UVRResolve String Var

class Resolvable a where
    resolveUVR :: a -> Reader UVRResolve a

instance Resolvable JinjaElem where
    resolveUVR jje = case jje of
        JustString t -> return (JustString t)
        UnresolvedVarRef n -> do
            UVRResolve uvrName resolveValue <- ask
            if uvrName == n
                then return (JustString (show resolveValue))
                else return (UnresolvedVarRef n)

instance Resolvable [JinjaElem] where
    resolveUVR :: [JinjaElem] -> Reader UVRResolve [JinjaElem]
    resolveUVR = traverse resolveUVR

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

instance Resolvable [Var] where
    resolveUVR :: [Var] -> Reader UVRResolve [Var]
    resolveUVR = traverse resolveUVR

instance Resolvable Var where
    resolveUVR varThatMightContainUVR = case varThatMightContainUVR of
        VarContainingJinja jjeVar -> do
            inserted <- resolveUVR jjeVar
            case jjeAllResolvedToString inserted of
                Left jjes -> return (VarContainingJinja jjes)
                Right s -> return (SimpleVarString s)
        ListVar varList -> do
            varList' <- resolveUVR varList
            return (ListVar varList')
        DictVar msv -> do
            msv' <- traverse resolveUVR msv
            return (DictVar msv')
        SimpleVarBool b -> return (SimpleVarBool b)
        SimpleVarFloat f -> return (SimpleVarFloat f)
        SimpleVarInt i -> return (SimpleVarInt i)
        SimpleVarString s -> return (SimpleVarString s)

instance Resolvable (Map String Var) where
    resolveUVR :: Map String Var -> Reader UVRResolve (Map String Var)
    resolveUVR = traverse resolveUVR

unrollLoopBasic :: KWLoop -> ModDecl -> [ModDecl]
unrollLoopBasic _kwLoop modDecl = let
    lpValLength = case loopList _kwLoop of
        (ListVar vars) -> length vars
        _ -> error "ERROR: lpVals must be ListVar!"
    in replicate lpValLength modDecl

instance Resolvable ModDecl where
    resolveUVR :: ModDecl -> Reader UVRResolve ModDecl
    resolveUVR modDecl = case modDecl of
        (GenericModDecl _name msv) -> do
            msv' <- resolveUVR msv
            return (GenericModDecl _name msv')
        _ -> error "ERROR: resolveUVR not implemented yet for non-generic modDecls!"

resolveUnrolledModDeclWithLoopVar :: [ModDecl] -> Reader KWLoop [ModDecl]
resolveUnrolledModDeclWithLoopVar mds = do
    _kwLoop <- ask
    let lpVals = case loopList _kwLoop of
            (ListVar vars) -> vars
            _ -> error "ERROR: lpVals must be ListVar!"
    let lpVar = case loopVar _kwLoop of
            (SimpleVarString s) -> s
            _ -> error "ERROR: lpVar must be SimpleVarString!"
    let rUVR = Prelude.map (UVRResolve lpVar) lpVals
    mapM (\(u, m) -> return $ runReader (resolveUVR m) u) (zip rUVR mds)

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
    let rUVR = Prelude.map (UVRResolve _indexVar . SimpleVarInt) [0..numberOfLoopVals-1]
    mapM (\(u, m) -> return $ runReader (resolveUVR m) u) (zip rUVR mds)
