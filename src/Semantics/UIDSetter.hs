{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
module Semantics.UIDSetter where

import GrammarTypes.AnsibleGrammarTypes
import Control.Monad.Reader (Reader, MonadReader (ask), local)
import Data.List.NonEmpty (toList, fromList)
import Control.Monad (zipWithM)

class UIDSettable a where
    setUID :: a -> Reader UID a

buildUID :: UID -> UID -> String -> UID
buildUID (SetUID s1) UnsetUID s2 = SetUID (s1 ++ "_" ++ s2)
buildUID (SetUID _) (SetUID _) _ = error "ERROR: buildUID was called on a SetUID as the child!"
buildUID UnsetUID _ _ = error "ERROR: buildUID was called with an UnsetUID as the parent!"

zipper :: UID -> String -> Int -> Task -> Reader UID Task
zipper parentUID s seqNum th =
    let _thUID = case th of
          Atomic _ _ uid' -> uid'
          ContainingBlock _ _ uid' -> uid'
        indivUID = buildUID parentUID _thUID (s ++ show seqNum)
     in local (const indivUID) (setUID th)

instance UIDSettable [Task] where
  setUID :: [Task] -> Reader UID [Task]
  setUID thl = do
    parentUID <- ask -- e.g. "r1_tasks_myfile_task"
    let sequenceNumbers = [0..]
    let x = zipWith (zipper parentUID "TH") sequenceNumbers thl
    sequence x

instance UIDSettable Task where
    setUID :: Task -> Reader UID Task
    setUID (Atomic attSet modDecl _) = do
        fullUID <- ask
        return (Atomic attSet modDecl fullUID)
    setUID (ContainingBlock attSet (Block _blockMain _rescue _always) _) = do
        blockUID <- ask
        let blockMainList = toList _blockMain
        let sequenceNumbers = [0..]
        bm <- zipWithM (zipper blockUID "THBlock") sequenceNumbers blockMainList
        rsc <- case _rescue of
                Nothing -> return []
                Just rsc' -> let
                    rescueList = toList rsc'
                    in zipWithM (zipper blockUID "THRescue") sequenceNumbers rescueList
        let mRSC = if null rsc then Nothing else Just rsc
        alw <- case _rescue of
                Nothing -> return []
                Just alw' -> let
                    alwaysList = toList alw'
                    in zipWithM (zipper blockUID "THAlways") sequenceNumbers alwaysList
        let mALW = if null alw then Nothing else Just alw
        return (ContainingBlock attSet (Block{
            blockMain = fromList bm,
            rescue = fmap fromList mRSC,
            always = fmap fromList mALW
        }) blockUID)

instance UIDSettable Play where
    setUID :: Play -> Reader UID Play
    setUID p = do
        _tasks <- traverse (local (const (SetUID "tasks")) . setUID) (tasks p)
        _handlers <- traverse (local (const (SetUID "handlers")) . setUID) (handlers p)
        return p {tasks=_tasks, handlers=_handlers}



