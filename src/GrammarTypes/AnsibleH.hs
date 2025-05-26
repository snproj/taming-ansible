{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
module GrammarTypes.AnsibleH
    (
        RootDir(..),
        Play(..),
        Role(..),
        JinjaPhrase(..),
        JinjaStringElem(..),
        JBE_EXP(..),
        JBE_BINOP(..),
        JBE_TEST(..),
        Var(..),
        AtomicAttributeSet(..),
        BlockAttributeSet(..),
        KWLoop(..),
        Task(..),
        Block(..),
        ModDecl(..),
        UID(..),
        mustBeString,
    ) where

import Data.List.NonEmpty
import Data.Map
import Data.Set
import qualified Data.Map as Map
import Data.List (elemIndices)

import GHC.Generics
import Data.Hashable

-------------------------------------------
--
--         UTIL TYPES
--
-------------------------------------------
-- data NonEmptySet a = NonEmptySet a (Set a)




-------------------------------------------
--
--         PLAY AND PLAYBOOK
--
-------------------------------------------
data RootDir = RootDir {
    playbook :: [Play],
    roledir :: Map String Role,
    looseTaskFiles :: Map String [Task]
} deriving (Generic, Show, Eq, Ord)

data Play = Play {
    vars :: Map String Var,
    tasks :: [Task],
    handlers :: [Task],
    roleNames :: [String]
} deriving (Generic, Show, Eq, Ord)

-------------------------------------------
--
--         ROLE
--
-------------------------------------------
-- TODO: Does not prevent multiple of the same directory! Find equivalent to Data.Set.NonEmpty!
data Role = Role {
    tasksDir :: Map String [Task],
    handlersDir :: Map String [Task]
} deriving (Generic, Show, Eq, Ord)


-------------------------------------------
--
--         TASK AND HANDLER
--
-------------------------------------------
-- data TaskMarker = TaskMarker
-- data HandlerMarker = HandlerMarker

data UID = SetUID String | UnsetUID
    deriving (Generic, Show, Eq, Ord)
instance Hashable UID

data Task
  = Atomic {
        atomicAttributeSet :: AtomicAttributeSet,
        modDecl :: ModDecl,
        uid :: UID
    }
  | Blocktask {
        blockAttributeSet :: BlockAttributeSet,
        block :: Block,
        uid :: UID
    }
  deriving (Generic, Show, Eq, Ord)
instance Hashable Task

-------------------------------------------
--
--         ATTRIBUTES
--
-------------------------------------------
data AtomicAttributeSet = AtomicAttributeSet {
    atomicNotify :: [Var],
    atomicLoop :: Maybe KWLoop,
    atomicListen :: [Var],
    atomicVars :: Map String Var,
    atomicWhen :: JBE_EXP,
    atomicIgnoreErrors :: Bool
} deriving (Generic, Eq, Ord, Show)
instance Hashable AtomicAttributeSet

-- instance Show AtomicAttributeSet where
--     show _ = "<ATTSET>"

data BlockAttributeSet = BlockAttributeSet {
    blockNotify :: [Var],
    blockVars :: Map String Var
} deriving (Generic, Eq, Ord)
instance Hashable BlockAttributeSet

instance Show BlockAttributeSet where
  show _ = "<ATTSET>"

data KWLoop = KWLoop {
    loopList :: [Var],
    loopVar :: Var, -- if `Nothing`, default will be evaluated eventually to "item"
    indexVar :: Maybe Var
} deriving (Generic, Show, Eq, Ord)
instance Hashable KWLoop


data Block = Block {
    blockMain :: [Task],
    rescue :: [Task],
    always :: [Task],
    goalkeeper :: Maybe Task
} deriving (Generic, Show, Eq, Ord)
instance Hashable Block



data ModDecl
    = GenericModDecl String (Map String Var)
    | ImportTasks Var
    | ImportRole
        { name :: Var
        , tasksFrom :: Var
        , handlersFrom :: Var
        }
    deriving (Generic, Show, Eq, Ord)
instance Hashable ModDecl


-------------------------------------------
--
--         VAR
--
-------------------------------------------
data Var
    = SimpleVarBool Bool
    | SimpleVarString String
    | VarContainingJinja JinjaPhrase
    deriving (Generic, Show, Eq, Ord)
instance Hashable Var

mustBeString :: Var -> String
mustBeString (SimpleVarString s) = s
mustBeString _ = error "ERROR: mustBeString called on a non-string var!"
-------------------------------------------
--
--         JINJA
--
-------------------------------------------


data JinjaPhrase
    = AllEventuallyString [JinjaStringElem]
    | JBEPhrase JBE_EXP
    | SingletonUVR String
    deriving (Generic, Show, Eq, Ord)
instance Hashable JinjaPhrase

data JinjaStringElem = JSE_STRING String | JSE_UVR String
    deriving (Generic, Show, Eq, Ord)
instance Hashable JinjaStringElem

data JBE_EXP
    = JBE_EXP_REGTEST UID JBE_TEST
    | JBE_EXP_BINARYOP JBE_EXP JBE_BINOP JBE_EXP
    | JBE_EXP_NOT JBE_EXP
    | JBE_EXP_PARENEXP JBE_EXP
    | JBE_EXP_PRIM Bool
    deriving (Generic, Show, Eq, Ord)
instance Hashable JBE_EXP

data JBE_BINOP
    = JBE_OP_AND
    | JBE_OP_OR
    deriving (Generic, Show, Eq, Ord)
instance Hashable JBE_BINOP

data JBE_TEST
    = JBE_TEST_SUCCEEDED
    | JBE_TEST_FAILED
    | JBE_TEST_DEFINED
    deriving (Generic, Show, Eq, Ord)
instance Hashable JBE_TEST

