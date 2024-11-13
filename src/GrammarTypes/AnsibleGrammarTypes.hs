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
        -- CompulsoryRoleDir(..),
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
        -- Task(..),
        -- Handler(..),
        -- Marker(..),
        TH(..),
        Block(..),
        -- Rescue(..),
        -- Always(..),
        ModDecl(..),
        TaskMarker(..),
        HandlerMarker(..),
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
    playbook :: Playbook,
    roledir :: Maybe (Map String Role),
    looseTaskFiles :: Maybe (Map String [TH TaskMarker]) -- all loose files assumed to be tasks
} deriving (Generic, Show, Eq, Ord)

-- data RoleDir = RoleDir (Maybe (Map String Role))

-- data LooseTaskFiles = LooseTaskFiles (Maybe (Map String [TH TaskMarker]))

-- data LooseHandlerFiles = LooseHandlerFiles (Maybe (Map String [TH HandlerMarker]))

data Playbook = PlaybookDefinedHere (NonEmpty Play)
    deriving (Generic, Show, Eq, Ord)
-- newtype ImportPlaybook = ImportPlaybook String -- TODO: should be path?

data Play = Play {
    hostPattern :: HostPattern,
    attributeSet :: AttributeSet,
    tasks :: Maybe [TH TaskMarker],
    handlers :: Maybe [TH HandlerMarker],
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
    tasksDir :: Maybe (Map RoleSubDirFileName [TH TaskMarker]),
    handlersDir :: Maybe (Map RoleSubDirFileName [TH HandlerMarker])
} deriving (Generic, Show, Eq, Ord)

-- data SumCompulsoryRoleDir where
--     SumCompulsoryRoleDir :: CompulsoryRoleDir a -> SumCompulsoryRoleDir
--     deriving (Generic, Show, Eq, Ord)

-- data CompulsoryRoleDir a = CompulsoryRoleDir (Map RoleSubDirFileName [TH a])
--     deriving (Generic, Show, Eq, Ord)

-- data CompulsoryRoleDir = CompulsoryRoleDir {
--     tasksDir :: Maybe (Map RoleSubDirFileName [TH TaskMarker]),
--     handlersDir :: Maybe (Map RoleSubDirFileName [TH HandlerMarker])
-- } deriving (Generic, Show, Eq, Ord)

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
    kwWhen :: Var,
    kwVars :: Maybe Var,
    kwChangedWhen :: Maybe Var,
    kwFailedWhen :: Maybe Var,
    kwUntil :: Maybe Var,
    kwRetries :: Var,
    kwRegister :: Maybe String,
    kwIgnoreErrors :: Var
} deriving (Generic, Eq, Ord)

instance Show AttributeSet where
    show :: AttributeSet -> String
    show _ = "<ATTSET>"


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

-- class Marker a
-- instance Marker TaskMarker
-- data family Marker a
data TaskMarker = TaskMarker
data HandlerMarker = HandlerMarker

-- data instance Marker TaskMarker
-- data instance Marker HandlerMarker
-- newtype Task = Task TH TaskMarker
-- newtype Handler = Handler TH HandlerMarker

data TH a
    = Atomic AttributeSet ModDecl
    | ContainingBlock AttributeSet (Block a)
    deriving (Generic, Show, Eq, Ord)

data Block a = Block {
    blockMain :: NonEmpty (TH a),
    rescue :: Maybe (NonEmpty (TH a)),
    always :: Maybe (NonEmpty (TH a))
} deriving (Generic, Show, Eq, Ord)

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