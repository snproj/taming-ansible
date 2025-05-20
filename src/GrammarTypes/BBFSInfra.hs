module GrammarTypes.BBFSInfra where
import GrammarTypes.BBFS (Expr (..), FS (..), Path (..))
import Data.Map

successRoot :: String
successRoot = "success"

runRoot :: String
runRoot = "success"

failureRoot :: String
failureRoot = "failure"

writeAbstractPath :: String -> Bool -> String -> Expr
writeAbstractPath abstractRoot val u = Trans (FS (fromList [(
    Path [
        abstractRoot,
        u],
    val)]))


mR :: String -> Expr
mR = writeAbstractPath runRoot True

mS :: String -> Expr
mS = writeAbstractPath runRoot True

mF :: String -> Expr
mF = writeAbstractPath failureRoot True

nop :: Expr
nop = Trans (FS empty)