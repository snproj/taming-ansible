module GrammarTypes.TBBFS where
import Data.Map

newtype TemplatablePath = TemplatablePath [PathPart] deriving (Show, Eq, Ord)

data PathPart = LitString String | TSubP String deriving (Show, Eq, Ord)

newtype FS = FS (Map TemplatablePath TemplatableBool) deriving (Show, Eq, Ord)

data TemplatableBool = LitBool Bool | TSubB String deriving (Show, Eq, Ord)

data Expr
    = Err
    | Ask FS Expr Expr
    | Seq Expr Expr
    | Trans FS
    | TChoice String (Map String Expr)
    deriving (Show, Eq, Ord)