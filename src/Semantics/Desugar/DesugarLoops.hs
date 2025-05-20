{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
module Semantics.Desugar.DesugarLoops where

import GrammarTypes.AnsibleH
import Data.Map (elems, Map, empty, insert, lookup, map, fromList, singleton, toList)
import Control.Monad.Reader (Reader, ask, MonadTrans (lift), runReader, local)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Data.Maybe (fromJust, mapMaybe, fromMaybe, isJust, catMaybes)
import qualified Text.Regex.TDFA.CorePattern as Data.List
import Control.Monad (zipWithM)
import Semantics.UIDSetter (buildUID)
import Semantics.StaticVarResolver (SymbolTable(SymbolTable), UVRResolvable (resolveContainedUVRs))
import Semantics.Desugar.DesugarBlocks (getSetUID)



getResolutions :: Task -> [SymbolTable]
getResolutions t = case t of
    Atomic {} -> let
        _kwLoop = fromJust $ atomicLoop $ atomicAttributeSet t
        loopPairs = zip (loopList _kwLoop) [0..]
        SimpleVarString _loopVarString = loopVar _kwLoop
        _indexVarString = case indexVar _kwLoop of
            Nothing -> "DUMMY"
            Just (SimpleVarString s) -> s
        in Prelude.map (\x -> createIndivSymbolTables x _loopVarString _indexVarString) loopPairs
    Blocktask {} -> error "ERROR: blocktask cannot contain loop!"
    where
        createIndivSymbolTables :: (Var, Int) -> String -> String -> SymbolTable
        createIndivSymbolTables (currLoopItem, currIdx) itemTemplateName idxTemplateName = SymbolTable (fromList [
            (itemTemplateName, currLoopItem),
            (idxTemplateName, SimpleVarString $ show currIdx)
            ])

adjustUIDs :: [Task] -> [Task]
adjustUIDs ts = zipWith (\t i -> t {uid=buildUID (uid t) ("loop" ++ show i)}) ts [0..]

getUnrolled :: Task -> [Task]
getUnrolled t = case t of
    Atomic {} -> let
        symbolTablesPerUnroll = getResolutions t
        unrolls = Prelude.map (runReader (resolveContainedUVRs t)) symbolTablesPerUnroll
        in adjustUIDs unrolls
    Blocktask {} -> error "ERROR: blocktask cannot contain loop!"

createLoopGoalkeeper :: Task -> [Task] -> Task
createLoopGoalkeeper t ts = Atomic {
        atomicAttributeSet = atomicAttributeSet t,
        modDecl = GenericModDecl "_lgk" (fromList (zipWith (\i ct -> ("s"++ show i, SimpleVarString (getSetUID ct))) [0..] ts)),
        uid = uid t
    }

rewriteRuleLoop :: Task -> [Task]
rewriteRuleLoop t = let
    unrolled = getUnrolled t
    in unrolled ++ [createLoopGoalkeeper t unrolled]


    -- where
    --     createParams :: [Task] -> Map String Var
    --     createParams ts = fromList

-- unrollLoopBasic :: KWLoop -> ModDecl -> [ModDecl]
-- unrollLoopBasic _kwLoop modDecl = let
--     lpValLength = case loopList _kwLoop of
--         (ListVar vars) -> length vars
--         _ -> error "ERROR: lpVals must be ListVar!"
--     in replicate lpValLength modDecl

-- resolveUnrolledModDeclWithLoopVar :: [ModDecl] -> Reader KWLoop [ModDecl]
-- resolveUnrolledModDeclWithLoopVar mds = do
--     _kwLoop <- ask
--     let lpVals = case loopList _kwLoop of
--             (ListVar vars) -> vars
--             _ -> error "ERROR: lpVals must be ListVar!"
--     let lpVar = case loopVar _kwLoop of
--             (SimpleVarString s) -> s
--             _ -> error "ERROR: lpVar must be SimpleVarString!"
--     let rUVR = Prelude.map (SymbolTable . Data.Map.singleton lpVar) lpVals
--     mapM (\(u, m) -> return $ runReader (resolveContainedUVRs m) u) (zip rUVR mds)

-- resolveUnrolledModDeclWithPause :: [ModDecl] -> Reader KWLoop [ModDecl]
-- resolveUnrolledModDeclWithPause mds = do
--     _kwLoop <- ask
--     let pauseSeconds = case pause _kwLoop of
--             Nothing -> error "ERROR: Called unroll loop with pause, but task does not have pause!"
--             Just pauseSeconds' -> pauseSeconds'
--     let pauseMod = GenericModDecl "GENERATED_pause" (Data.Map.fromList [("seconds", pauseSeconds)])
--     return (Data.List.NonEmpty.toList (intersperse pauseMod (head mds :| tail mds)))

-- resolveUnrolledModDeclWithIndexVar :: [ModDecl] -> Reader KWLoop [ModDecl]
-- resolveUnrolledModDeclWithIndexVar mds = do
--     _kwLoop <- ask
--     let numberOfLoopVals = case loopList _kwLoop of
--             (ListVar vars) -> length vars
--             _ -> error "ERROR: lpVals must be ListVar!"
--     let _indexVar = case indexVar _kwLoop of
--             Nothing -> error "ERROR: Called unroll loop with indexvar, but task does not have indexvar!"
--             Just _indexVar' -> case _indexVar' of
--                 SimpleVarString s -> s
--                 _ -> error "ERROR: _indexvar' must be SimpleVarString!"
--     let rUVR = Prelude.map ((SymbolTable . Data.Map.singleton _indexVar) . SimpleVarInt) [0 .. numberOfLoopVals-1]
--     mapM (\(u, m) -> return $ runReader (resolveContainedUVRs m) u) (zip rUVR mds)

-- createRegisterUnifierTask :: UID -> [Task] -> Task
-- createRegisterUnifierTask origTHUID thl = let
--     regsList = mapMaybe (atomicRegister . thAtomicAttributeSet) thl
--     regsAsVars = Prelude.map SimpleVarString regsList
--     in Atomic (thAtomicAttributeSet $ head thl) (GenericModDecl "unify_loop_ds_regs" (Data.Map.fromList [("loop_ds_task_regs", ListVar regsAsVars)])) (getUnifyRegUID origTHUID)

-- getUnifyRegUID :: UID -> UID
-- getUnifyRegUID origTHUID = buildUID origTHUID UnsetUID "unifyregs"

-- getReferencedUIDsFromJBE :: JBE_EXP -> [UID]
-- getReferencedUIDsFromJBE jbe = case jbe of
--     JBE_EXP_REGTEST (JBE_REG_R uid) _ _ -> [uid]
--     JBE_EXP_BINARYOP e1 _ e2 -> getReferencedUIDsFromJBE e1 ++ getReferencedUIDsFromJBE e2
--     JBE_EXP_UNARYOP _ e -> getReferencedUIDsFromJBE e
--     JBE_EXP_PARENEXP e -> getReferencedUIDsFromJBE e
--     _ -> []

-- applyToRegs :: (JBE_REG -> JBE_REG) -> JBE_EXP -> JBE_EXP
-- applyToRegs regf jbe = case jbe of
--     JBE_EXP_REGTEST reg top test -> JBE_EXP_REGTEST (regf reg) top test
--     JBE_EXP_BINARYOP e1 op e2 -> JBE_EXP_BINARYOP (applyToRegs regf e1) op (applyToRegs regf e2)
--     JBE_EXP_UNARYOP op e -> JBE_EXP_UNARYOP op (applyToRegs regf e)
--     JBE_EXP_PARENEXP e -> JBE_EXP_PARENEXP (applyToRegs regf e)
--     x -> x

-- replaceWhen :: Map UID UID -> Task -> Task
-- replaceWhen muu (Atomic aas modDecl uid) = let
--     VarContainingJinja (JBEPhrase jbe) = atomicWhen aas
--     w' = VarContainingJinja (JBEPhrase (replaceReg muu jbe))
--     in Atomic aas {atomicWhen= w'} modDecl uid
-- replaceWhen muu (ContainingBlock bas blk uid) = let
--     VarContainingJinja (JBEPhrase jbe) = blockWhen bas
--     w' = VarContainingJinja (JBEPhrase (replaceReg muu jbe))
--     in ContainingBlock bas {blockWhen = w'} blk uid

-- replaceReg :: Map UID UID -> JBE_EXP -> JBE_EXP
-- replaceReg muu jbe = let
--     ufs = Prelude.map (\(key, value) -> \x -> if x == key then value else x) (Data.Map.toList muu)
--     rfs = Prelude.map applyToReg ufs
--     jbe' = foldl (\acc f -> applyToRegs f acc) jbe rfs
--     in jbe'
--     where
--         applyToReg :: (UID -> UID) -> (JBE_REG -> JBE_REG)
--         applyToReg uf (JBE_REG_R uid) = JBE_REG_R (uf uid)

-- getReferencedTHsFromWhen :: Task -> Reader (Map UID Task) [Task]
-- getReferencedTHsFromWhen t = do
--     let VarContainingJinja (JBEPhrase w) = case t of
--             Atomic aas _ _ -> atomicWhen aas
--             ContainingBlock bas _ _ -> blockWhen bas
--     let uids = getReferencedUIDsFromJBE w
--     mut <- ask
--     let ts = mapMaybe (`Data.Map.lookup` mut) uids
--     return ts

-- unrollTH :: Task -> [Task]
-- unrollTH (Atomic aas modDecl uid) = let
--     ml = atomicLoop aas
--     in case ml of
--         Nothing -> [Atomic aas modDecl uid]
--         Just l -> let
--             modDecls = unrollLoopBasic l modDecl
--             modDecls' = runReader (resolveUnrolledModDeclWithLoopVar modDecls) l
--             modDecls'' = runReader (resolveUnrolledModDeclWithIndexVar modDecls') l
--             in Prelude.map (\mdcl -> Atomic aas mdcl uid) modDecls''
-- unrollTH (ContainingBlock bas blk uid) = let
--     bm = Data.List.NonEmpty.fromList $ concatMap unrollTH $ blockMain blk
--     r = fmap (Data.List.NonEmpty.fromList . concatMap unrollTH) (rescue blk)
--     a = fmap (Data.List.NonEmpty.fromList . concatMap unrollTH) (always blk)
--     in [ContainingBlock bas Block {blockMain=bm, rescue=r, always=a} uid]

-- getLoopyTasks :: [Task] -> [Task]
-- getLoopyTasks = Prelude.filter isLoopy
--     where
--     isLoopy :: Task -> Bool
--     isLoopy = isJust . atomicLoop . thAtomicAttributeSet

-- getUIDFromTask :: Task -> UID
-- getUIDFromTask (Atomic _ _ uid) = uid
-- getUIDFromTask (ContainingBlock _ _ uid) = uid

-- correctReferencesToLoopyTasks :: [Task] -> [Task] -> [Task]
-- correctReferencesToLoopyTasks loopies ts = let
--     loopyUIDs = Prelude.map getUIDFromTask loopies
--     muu = Data.Map.fromList $ Prelude.map (\u -> (u, getUnifyRegUID u)) loopyUIDs
--     corrected = Prelude.map (replaceWhen muu) ts
--     in corrected

-- unrollLoopsInPlay :: Play -> Play
-- unrollLoopsInPlay p = let
--     ts = tasks p
--     hs = handlers p
--     loopies = getLoopyTasks (ts ++ hs)
--     uts = concatMap unrollTH ts
--     uhs = concatMap unrollTH hs
--     cts = correctReferencesToLoopyTasks loopies uts
--     chs = correctReferencesToLoopyTasks loopies uhs
--     in p {tasks=cts, handlers=chs}


