{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
module Semantics.UIDSetter where

import GrammarTypes.AnsibleGrammarTypes
    ( Block(Block, always, blockMain, rescue),
      Task(Blocktask, Atomic),
      UID(..),
      Play(handlers, tasks) )
import Control.Monad.Reader (Reader, MonadReader (ask), local)
import Data.List.NonEmpty (toList, fromList)
import Control.Monad (zipWithM)

class UIDSettable a where
    setUID :: a -> Reader UID a

buildUID :: UID -> String -> UID
buildUID (SetUID s1) s2 = SetUID (s1 ++ "_" ++ s2)
-- buildUID (SetUID _) (SetUID _) _ = error "ERROR: buildUID was called on a SetUID as the child!"
buildUID UnsetUID _ = error "ERROR: buildUID was called with an UnsetUID as the parent!"

zipper :: UID -> String -> Int -> Task -> Reader UID Task
zipper parentUID s seqNum th =
    let indivUID = buildUID parentUID (s ++ show seqNum)
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
    setUID (Blocktask attSet (Block _blockMain _rescue _always _goalkeeper) _) = do
        blockUID <- ask
        let sequenceNumbers = [0..]
        bm <- zipWithM (zipper blockUID "THBlock") sequenceNumbers _blockMain
        rsc <- zipWithM (zipper blockUID "THRescue") sequenceNumbers _rescue
        alw <- zipWithM (zipper blockUID "THAlways") sequenceNumbers _always
        -- gk <- 
        return (Blocktask attSet (Block{
            blockMain = bm,
            rescue = rsc,
            always = alw
        }) blockUID)

instance UIDSettable Play where
    setUID :: Play -> Reader UID Play
    setUID p = do
        let sequenceNumbers = [0..]
        -- _tasks <- traverse (local (const (SetUID "tasks")) . setUID) (tasks p)
        _tasks <- zipWithM (zipper (SetUID "") "tasks") sequenceNumbers (tasks p)
        -- _handlers <- traverse (local (const (SetUID "handlers")) . setUID) (handlers p)
        _handlers <- zipWithM (zipper (SetUID "") "handlers") sequenceNumbers (handlers p)
        return p {tasks=_tasks, handlers=_handlers}



