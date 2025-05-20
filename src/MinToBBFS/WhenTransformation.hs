module MinToBBFS.WhenTransformation where
import GrammarTypes.BBFS (Expr (..), FS (..), Path (..))
import GrammarTypes.AnsibleMin (Task (..), AttributeSet (..), JBE_EXP (..), JBE_TEST (..), JBE_BINOP (..))
import Data.Map
import Data.Maybe
import GrammarTypes.BBFSInfra (nop, runRoot, successRoot, failureRoot)

whenTransformation :: Task -> Expr -> Expr
whenTransformation t expr = let
    w = (when . attributeSet) t
    
    in undefined

newtype Test = Test (String, JBE_TEST) deriving (Show, Eq, Ord)

lookupTest :: String -> JBE_TEST -> Map Test Bool -> Bool
lookupTest u test phi = let
    test' = Test (u, test)
    in fromJust $ Data.Map.lookup test' phi

shExpand :: [Test] -> Map Test Bool -> Expr -> JBE_EXP -> Expr
shExpand lamb phi m j
    | Prelude.null lamb && eval j phi = m
    | Prelude.null lamb && not (eval j phi) = nop
    | otherwise = let
        q = toQuery (head lamb)
        b1 = shExpand (tail lamb) (insert (head lamb) True phi) m j
        b2 = shExpand (tail lamb) (insert (head lamb) False phi) m j
        in Ask q b1 b2

toQuery :: Test -> FS
toQuery (Test (u, test)) = let
    rootPath = case test of
        JBE_TEST_DEFINED -> runRoot
        JBE_TEST_SUCCEEDED -> successRoot
        JBE_TEST_FAILED -> failureRoot
    in FS (fromList [(
        Path [rootPath, u],
        True
    )])

eval :: JBE_EXP -> Map Test Bool -> Bool
eval j phi = case j of
    JBE_EXP_REGTEST u test -> lookupTest u test phi
    JBE_EXP_BINARYOP ex1 op ex2 -> case op of
        JBE_OP_AND -> eval ex1 phi && eval ex2 phi
        JBE_OP_OR -> eval ex1 phi || eval ex2 phi
    JBE_EXP_NOT ex -> not (eval ex phi)
    JBE_EXP_PARENEXP ex -> eval ex phi
    JBE_EXP_PRIM b -> b
