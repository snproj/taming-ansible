module MinToBBFS.IgnoreErrorTransformation where
import GrammarTypes.BBFS
import GrammarTypes.AnsibleMin
import GrammarTypes.BBFSInfra (mF, nop, failureRoot)
import Data.Map

ignoreErrorTransformation :: Task -> Expr -> Expr
ignoreErrorTransformation t expr = let
    ie = (ignoreErrors . attributeSet) t
    u = uid t
    in if ie then
        fst (trav u expr)
    else
        expr

cFs :: String -> Expr -> Expr
cFs u m = Ask (FS (fromList [(
    Path [
        failureRoot,
        u],
    True)])) nop m

trav :: String -> Expr -> (Expr, Bool)
trav u expr = case expr of
    Seq ex1 ex2 -> case ex1 of
        Err -> (mF u, True)
        Ask q ifBranch elseBranch -> let
            (ifBranch', ifBranchHasErr) = trav u ifBranch
            (elseBranch', elseBranchHasErr) = trav u elseBranch
            (ex2', ex2HasErr) = trav u ex2
            branchesHaveErr = ifBranchHasErr || elseBranchHasErr
            hasErr = ex2HasErr || branchesHaveErr
            in if branchesHaveErr then
                (Seq (Ask q ifBranch' elseBranch') (cFs u ex2'), hasErr)
            else
                (Seq (Ask q ifBranch' elseBranch') ex2', hasErr)
        Seq _ _ -> error "ERROR: first term of Seq cannot also be Seq!" -- TODO: not strictly a necessary restriction?
        Trans fs -> let
            (ex2', ex2HasErr) = trav u ex2
            in (Seq (Trans fs) ex2', ex2HasErr)
    Ask q ifBranch elseBranch -> let
        (ifBranch', ifBranchHasErr) = trav u ifBranch
        (elseBranch', elseBranchHasErr) = trav u elseBranch
        branchesHaveErr = ifBranchHasErr || elseBranchHasErr
        in (Ask q ifBranch' elseBranch', branchesHaveErr)
    Err -> (mF u, True)
    Trans fs -> (Trans fs, False) -- False because end of linked list