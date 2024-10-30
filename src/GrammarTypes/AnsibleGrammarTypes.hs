{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
module GrammarTypes.AnsibleGrammarTypes
    (
        RootDir(..),
        Playbook(..),
        Play(..),
        HostPattern(..),
        Role(..),
        CompulsoryRoleDir(..),
        -- TasksFile(..),
        -- HandlersFile(..),
        RoleSubDirFileName(..),
        JinjaElem(..),
        Var(..),
        AttributeSet(..),
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
        Task(..),
        Handler(..),
        -- TH(..),
        ModDecl(..),
        BlockTask(..),
        RescueTask(..),
        AlwaysTask(..),
        BlockHandler(..),
        RescueHandler(..),
        AlwaysHandler(..),
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
data RootDir = RootDir Playbook (Maybe (Map String Role))
    deriving (Generic, Show, Eq, Ord)

data Playbook = PlaybookDefinedHere (NonEmpty Play)
    deriving (Generic, Show, Eq, Ord)
-- newtype ImportPlaybook = ImportPlaybook String -- TODO: should be path?

data Play = Play {
    hostPattern :: HostPattern,
    attributeSet :: AttributeSet,
    tasks :: Maybe [Task],
    handlers :: Maybe [Handler],
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
newtype Role = Role (NonEmpty CompulsoryRoleDir)
    deriving (Generic, Show, Eq, Ord)

data CompulsoryRoleDir
    = TasksDir (Map RoleSubDirFileName [Task])
    | HandlersDir (Map RoleSubDirFileName [Handler])
    deriving (Generic, Show, Eq, Ord)

data RoleSubDirFileName = MainName
    | OtherName String
    deriving (Generic, Show, Eq, Ord)








-------------------------------------------
--
--         JINJA
--
-------------------------------------------

data JinjaElem -- TODO: Formalize jinja lmao
    = JustString String -- this should instead be resolved into stuff within jinja double brackets
    -- | LoopTarget
    | UnresolvedVarRef String
    deriving (Generic, Show, Eq, Ord)

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
    | VarContainingJinja [JinjaElem]
    deriving (Generic, Show, Eq, Ord)

-------------------------------------------
--
--         ATTRIBUTES
--
-------------------------------------------
-- I've decided to sort out which attributes are valid for which grammar
-- constructs in the semantics instead
data AttributeSet = AttributeSet {
    kwName :: Maybe String,
    kwForceHandlers :: Maybe Var,
    kwNotify :: Maybe Var,
    kwLoop :: Maybe KWLoop,
    kwWhen :: Maybe Var,
    kwVars :: Maybe Var,
    kwChangedWhen :: Maybe Var,
    kwFailedWhen :: Maybe Var,
    kwUntil :: Maybe Var,
    kwRetries :: Maybe Var,
    kwRegister :: Maybe String
} deriving (Generic, Show, Eq, Ord)


data KWLoop = KWLoop {
    loopList :: Var,
    loopVar :: Var, -- if `Nothing`, default will be evaluated eventually to "item"
    indexVar :: Maybe Var,
    pause :: Maybe Var
} deriving (Generic, Show, Eq, Ord)





-------------------------------------------
--
--         TASK AND HANDLER
--
-------------------------------------------
-- class TaskOrHandler a
-- instance TaskOrHandler Task
-- instance TaskOrHandler Handler

-- might need to bring back TaskMarker if this recursive definition comes back
-- to bite us in the behind
data Task
    = AtomicTask AttributeSet ModDecl
    | TaskContainingABlock AttributeSet BlockTask
    deriving (Generic, Show, Eq, Ord)

data Handler
    = AtomicHandler AttributeSet ModDecl
    | HandlerContainingABlock AttributeSet BlockHandler
    deriving (Generic, Show, Eq, Ord)
    
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









-------------------------------------------
--
--         BLOCK, RESCUE AND ALWAYS
--
-------------------------------------------
data BlockTask = BlockTask (NonEmpty Task) (Maybe RescueTask)
  deriving (Generic, Show, Eq, Ord)
data RescueTask = RescueTask (NonEmpty Task) (Maybe AlwaysTask)
  deriving (Generic, Show, Eq, Ord)
newtype AlwaysTask = AlwaysTask (NonEmpty Task)
    deriving (Generic, Show, Eq, Ord)


data BlockHandler = BlockHandler (NonEmpty Handler) (Maybe RescueHandler)
  deriving (Generic, Show, Eq, Ord)
data RescueHandler = RescueHandler (NonEmpty Handler) (Maybe AlwaysHandler)
  deriving (Generic, Show, Eq, Ord)
newtype AlwaysHandler = AlwaysHandler (NonEmpty Handler)
  deriving (Generic, Show, Eq, Ord)