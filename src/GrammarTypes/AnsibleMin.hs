module GrammarTypes.AnsibleMin
    (
        Task(..),
        ModDecl(..),
        Var(..),
        AttributeSet(..),
        JBE_EXP(..),
        JBE_BINOP(..),
        JBE_TEST(..),
    ) where
import Data.Map

data Task = Task {
    attributeSet :: AttributeSet,
    modDecl :: ModDecl,
    uid :: String
}deriving (Show, Eq, Ord)

data ModDecl = ModDecl {
    name :: String,
    params :: Map String Var
}deriving (Show, Eq, Ord)

data Var = SimpleVarBool Bool | SimpleVarString String deriving (Show, Eq, Ord)

data AttributeSet = AttributeSet {
    when :: JBE_EXP,
    ignoreErrors :: Bool
}deriving (Show, Eq, Ord)

data JBE_EXP
    = JBE_EXP_REGTEST String JBE_TEST
    | JBE_EXP_BINARYOP JBE_EXP JBE_BINOP JBE_EXP
    | JBE_EXP_NOT JBE_EXP
    | JBE_EXP_PARENEXP JBE_EXP
    | JBE_EXP_PRIM Bool
    deriving (Show, Eq, Ord)

data JBE_BINOP
    = JBE_OP_AND
    | JBE_OP_OR
    deriving (Show, Eq, Ord)

data JBE_TEST
    = JBE_TEST_SUCCEEDED
    | JBE_TEST_FAILED
    | JBE_TEST_DEFINED
    deriving (Show, Eq, Ord)