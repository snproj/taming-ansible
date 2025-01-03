{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
module GrammarTypes.AnsibleGrammarTypes
    (
        RootDir(..),
        Play(..),
        HostPattern(..),
        Role(..),
        -- CompulsoryRoleDir(..),
        -- TasksFile(..),
        -- HandlersFile(..),
        RoleSubDirFileName(..),
        -- JinjaElem(..),
        JinjaPhrase(..),
        JinjaStringElem(..),
        JBE_EXP(..),
        -- JBE_PRIM(..),
        JBE_REG(..),
        JBE_TOP(..),
        JBE_OP(..),
        JBE_TEST(..),
        JBE_UNIMPL(..),
        Var(..),
        PlayAttributeSet(..),
        AtomicAttributeSet(..),
        BlockAttributeSet(..),
        -- KWForceHandlers,
        -- KWNotify,
        KWLoop(..),
        -- LoopList(..),
        -- KWWhen,
        -- KWVars,
        -- KWChangedWhen,
        -- KWFailedWhen,
        -- KWUntil,
        -- KWRetries,
        -- KWRegister,
        -- Task(..),
        -- Handler(..),
        -- Marker(..),
        -- TH(..),
        Task(..),
        Block(..),
        -- Rescue(..),
        -- Always(..),
        ModDecl(..),
        -- TaskMarker(..),
        -- HandlerMarker(..),
        UID(..),
        -- BlockTask(..),
        -- RescueTask(..),
        -- AlwaysTask(..),
        -- BlockHandler(..),
        -- RescueHandler(..),
        -- AlwaysHandler(..),
        -- UnresolvedVarRef(..),
        -- RawJJEVar(..),
        -- IsTaskOrHandler
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
data NonEmptySet a = NonEmptySet a (Set a)




-------------------------------------------
--
--         PLAY AND PLAYBOOK
--
-------------------------------------------
data RootDir = RootDir {
    playbook :: NonEmpty Play,
    roledir :: Maybe (Map String Role),
    looseTaskFiles :: Maybe (Map String [Task]) -- all loose files assumed to be tasks
} deriving (Generic, Show, Eq, Ord)

data Play = Play {
    hostPattern :: HostPattern,
    playAttributeSet :: PlayAttributeSet,
    tasks :: [Task],
    handlers :: [Task],
    roleNames :: Maybe [String]
} deriving (Generic, Show, Eq, Ord)

data HostPattern = UnionHosts HostPattern HostPattern
    | SetdiffHosts HostPattern HostPattern
    | IntersectHosts HostPattern HostPattern
    | HostSet (Set String)
    deriving (Generic, Show, Eq, Ord)




-------------------------------------------
--
--         ROLE
--
-------------------------------------------
-- TODO: Does not prevent multiple of the same directory! Find equivalent to Data.Set.NonEmpty!
data Role = Role {
    tasksDir :: Maybe (Map RoleSubDirFileName [Task]),
    handlersDir :: Maybe (Map RoleSubDirFileName [Task])
} deriving (Generic, Show, Eq, Ord)

data RoleSubDirFileName = MainName
    | OtherName String
    deriving (Generic, Show, Eq, Ord)








-------------------------------------------
--
--         JINJA
--
-------------------------------------------

data JinjaStringElem = JSE_STRING String | JSE_UVR String
    deriving (Generic, Show, Eq, Ord)
instance Hashable JinjaStringElem

data JinjaPhrase
    = AllEventuallyString [JinjaStringElem]
    | JBEPhrase JBE_EXP
    | SingletonUVR String
    deriving (Generic, Show, Eq, Ord)
instance Hashable JinjaPhrase

-- data JinjaElem -- TODO: Formalize jinja lmao
--     = JustString String -- this should instead be resolved into stuff within jinja double brackets
--     | UnresolvedVarRef String
--     | JinjaBooleanExp JBE_EXP
--     deriving (Generic, Show, Eq, Ord)
-- instance Hashable JinjaElem

data JBE_EXP
    = JBE_EXP_REGTEST JBE_REG JBE_TOP JBE_TEST
    | JBE_EXP_BINARYOP JBE_EXP JBE_OP JBE_EXP
    | JBE_EXP_UNARYOP JBE_OP JBE_EXP
    | JBE_EXP_PARENEXP JBE_EXP
    | JBE_EXP_PRIM Bool
    | JBE_EXP_UVR String
    | JBE_EXP_UNIMPL String
    deriving (Generic, Show, Eq, Ord)
instance Hashable JBE_EXP

-- data JBE_PRIM = JBE_PRIM_TRUE | JBE_PRIM_FALSE
--     deriving (Generic, Show, Eq, Ord)
-- instance Hashable JBE_PRIM
data JBE_REG = JBE_REG_R String -- TODO: change to Task
    deriving (Generic, Show, Eq, Ord)
instance Hashable JBE_REG
data JBE_TOP = JBE_TOP_IS
    deriving (Generic, Show, Eq, Ord)
instance Hashable JBE_TOP
data JBE_OP
    = JBE_OP_AND
    | JBE_OP_OR
    | JBE_OP_NOT
    deriving (Generic, Show, Eq, Ord)
instance Hashable JBE_OP
data JBE_TEST
    = JBE_TEST_SUCCEEDED
    | JBE_TEST_FAILED
    | JBE_TEST_DEFINED
    | JBE_TEST_CHANGED
    deriving (Generic, Show, Eq, Ord)
instance Hashable JBE_TEST
data JBE_UNIMPL = JBE_UNIMPL
    deriving (Generic, Show, Eq, Ord)
instance Hashable JBE_UNIMPL

-- data RawJJEVar = ListOfJJEs [JinjaElem] | SimpleJJE JinjaElem
--     deriving (Generic, Show, Eq, Ord)

-- instance Show JinjaElem where
--     show :: JinjaElem -> String
--     show (JustString s) = s
--     show LoopTarget = jinjaLoopTargetMagicString
--         where jinjaLoopTargetMagicString = "{item}"


-------------------------------------------
--
--         VAR
--
-------------------------------------------
-- data VarDecl = VarDecl String Var
-- newtype ListVarOnly = ListVarOnly [Var]
data Var
    = ListVar [Var]
    | DictVar (Map String Var)
    | SimpleVarBool Bool
    | SimpleVarString String
    | SimpleVarInt Int
    | SimpleVarFloat Float
    | VarContainingJinja JinjaPhrase
    deriving (Generic, Show, Eq, Ord)
instance Hashable Var

-------------------------------------------
--
--         ATTRIBUTES
--
-------------------------------------------
data PlayAttributeSet = PlayAttributeSet {
    playVars :: Maybe Var
} deriving (Generic, Show, Eq, Ord)
instance Hashable PlayAttributeSet

data AtomicAttributeSet = AtomicAttributeSet {
    atomicNotify :: Maybe Var,
    atomicLoop :: Maybe KWLoop,
    atomicWhen :: Var,
    atomicVars :: Maybe Var,
    atomicChangedWhen :: Maybe Var,
    atomicFailedWhen :: Maybe Var,
    atomicUntil :: Maybe Var,
    atomicRetries :: Var,
    atomicRegister :: Maybe String,
    atomicIgnoreErrors :: Var,
    atomicListen :: Maybe Var
} deriving (Generic, Eq, Ord)
instance Hashable AtomicAttributeSet

instance Show AtomicAttributeSet where
    show _ = "<ATTSET>"

-- I've decided to sort out which attributes are valid for which grammar
-- constructs in the semantics instead
data BlockAttributeSet = BlockAttributeSet {
    blockNotify :: Maybe Var,
    blockWhen :: Var,
    blockVars :: Maybe Var
} deriving (Generic, Eq, Ord)
instance Hashable BlockAttributeSet

instance Show BlockAttributeSet where
  show _ = "<ATTSET>"

data KWLoop = KWLoop {
    loopList :: Var,
    loopVar :: Var, -- if `Nothing`, default will be evaluated eventually to "item"
    indexVar :: Maybe Var,
    pause :: Maybe Var
} deriving (Generic, Show, Eq, Ord)
instance Hashable KWLoop




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
        thAtomicAttributeSet :: AtomicAttributeSet,
        thModDecl :: ModDecl,
        thUID :: UID
    }
  | ContainingBlock {
        thBlockAttributeSet :: BlockAttributeSet,
        thBlock :: Block,
        thUID :: UID
    }
  deriving (Generic, Show, Eq, Ord)
instance Hashable Task


data Block = Block {
    blockMain :: NonEmpty Task,
    rescue :: Maybe (NonEmpty Task),
    always :: Maybe (NonEmpty Task)
} deriving (Generic, Show, Eq, Ord)
instance Hashable Block

-- data Block a = Block (NonEmpty (TH a)) (Maybe (Rescue a))
--   deriving (Generic, Show, Eq, Ord)

-- data Rescue a = Rescue (NonEmpty (TH a)) (Maybe (Always a))
--   deriving (Generic, Show, Eq, Ord)

-- newtype Always a = Always (NonEmpty (TH a))
--   deriving (Generic, Show, Eq, Ord)

-- data Task
--     = AtomicTask AttributeSet ModDecl
--     | TaskContainingABlock AttributeSet BlockTask
--     deriving (Generic, Show, Eq, Ord)

-- data Handler
--     = AtomicHandler AttributeSet ModDecl
--     | HandlerContainingABlock AttributeSet BlockHandler
--     deriving (Generic, Show, Eq, Ord)
    
-- class IsTaskOrHandler a
-- instance IsTaskOrHandler Task
-- instance IsTaskOrHandler Handler


data ModDecl
    = GenericModDecl String (Map String Var)
    | ImportTasks Var
    | IncludeTasks
        { apply :: Maybe Var
        , file :: Var
        }
    | ImportRole
        { name :: Var
        , tasks_from :: Var
        , handlers_from :: Var
        }
    | IncludeRole
        { apply :: Maybe Var
        , name :: Var
        , tasks_from :: Var
        , handlers_from :: Var
        }
    deriving (Generic, Show, Eq, Ord)
instance Hashable ModDecl








-------------------------------------------
--
--         BLOCK, RESCUE AND ALWAYS
--
-------------------------------------------
-- data BlockTask = BlockTask (NonEmpty Task) (Maybe RescueTask)
--   deriving (Generic, Show, Eq, Ord)
-- data RescueTask = RescueTask (NonEmpty Task) (Maybe AlwaysTask)
--   deriving (Generic, Show, Eq, Ord)
-- newtype AlwaysTask = AlwaysTask (NonEmpty Task)
--     deriving (Generic, Show, Eq, Ord)


-- data BlockHandler = BlockHandler (NonEmpty Handler) (Maybe RescueHandler)
--   deriving (Generic, Show, Eq, Ord)
-- data RescueHandler = RescueHandler (NonEmpty Handler) (Maybe AlwaysHandler)
--   deriving (Generic, Show, Eq, Ord)
-- newtype AlwaysHandler = AlwaysHandler (NonEmpty Handler)
--   deriving (Generic, Show, Eq, Ord)