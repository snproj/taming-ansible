module GrammarTypes.TBBFSInfra where
import GrammarTypes.TBBFS (Expr (..), FS (..),)
import Data.Map


nop :: Expr
nop = Trans (FS empty)