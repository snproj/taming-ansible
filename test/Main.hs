module Main (main) where
import MinToBBFS.IgnoreErrorTransformation (trav)
import qualified GrammarTypes.BBFS
import GrammarTypes.TBBFS
import Data.Map
import Lexer.DirectoryStacker (gatherDir)
import Lexer.CombinedParser (parseRootDir)
import Semantics.UIDSetter (setUIDsForRD)
import Semantics.Desugar.DesugarRoles (rewriteRuleInlineRolesForRD)
import Semantics.Desugar.DesugarImports (rewriteRuleImports)
import Semantics.Desugar.DesugarBlocks (rewriteRuleBlockForRD)
import Semantics.Desugar.DesugarHandlers (rewriteRuleHandlerForRD)
import GrammarTypes.AnsibleH (RootDir)
import Semantics.StaticVarResolver (rewriteRuleVarForRD)
import MinToBBFS.MinToBBFS (Omega(..), Translatable (toBBFS))
import GrammarTypes.TBBFSInfra (nop)
import GrammarTypes.HToMin (convertToMin)
import Control.Monad.Reader
import SymbolicExecution.BBFSSymSem (idempotencyCheck)
import GrammarTypes.BBFS (checkIntegrity)
import SymbolicExecution.Utils (getStartingFS)
import System.Environment (getArgs)
import System.Directory

readAnsible :: String -> String -> IO RootDir
readAnsible absoluteProjectPath playbookFileName = do
    (name, dir) <- gatherDir absoluteProjectPath
    -- print dir
    let rd = parseRootDir playbookFileName dir
    -- print rd
    let uidrd = setUIDsForRD rd
    return uidrd

checkOsTExpr :: Expr
checkOsTExpr = Ask (FS (fromList [(TemplatablePath [TSubP "target"], LitBool True)])) nop Err

fileExprTExpr :: Expr
fileExprTExpr = TChoice "state" (fromList [
    ("absent", Trans (FS (fromList [(TemplatablePath [TSubP "path"], LitBool False)]))),
    ("present", Trans (FS (fromList [(TemplatablePath [TSubP "path"], LitBool True)])))
    ])

assertFileExistsTExpr :: Expr
assertFileExistsTExpr = Ask (FS (fromList [(TemplatablePath [TSubP "path"], LitBool True)])) nop Err

gitCloneTExpr :: Expr
gitCloneTExpr = Ask (FS (fromList [(TemplatablePath [TSubP "to"],LitBool True)])) Err (Trans (FS (fromList [(TemplatablePath [TSubP "to"],LitBool True)])))

installLibTExpr :: Expr
installLibTExpr = Ask (FS (fromList [(TemplatablePath [TSubP "prereq_install_loc"],LitBool True)])) (Trans (FS (fromList [(TemplatablePath [LitString "lib"],LitBool True)]))) Err

singletonFS :: String -> Bool -> FS
singletonFS s b = FS $ fromList [(TemplatablePath [TSubP s],LitBool b)]
sa :: FS
sa = singletonFS "a" True
sb :: FS
sb = singletonFS "b" True
sr :: FS
sr = singletonFS "r" True
br :: Expr
br = Ask sb nop (Ask sr nop Err)
ba :: Expr
ba = Ask sb (Ask sa nop Err) Err
b :: Expr
b = Ask sb nop Err
bra :: Expr
bra = Ask sa br Err
gkTExpr :: Expr
gkTExpr = TChoice "mode" $ fromList [
    ("BRA",bra),
    ("BA",ba),
    ("BR",br),
    ("B",b)
    ]

omega :: Omega
omega = Omega { mse = fromList [
    ("check_os",checkOsTExpr),
    ("file",fileExprTExpr),
    ("assert_file_exists",assertFileExistsTExpr),
    ("git_clone",gitCloneTExpr),
    ("install_lib",installLibTExpr),
    ("_gk",gkTExpr)
] }

desugar :: RootDir -> RootDir
desugar = rewriteRuleHandlerForRD.rewriteRuleHandlerForRD.rewriteRuleBlockForRD.rewriteRuleVarForRD.rewriteRuleImports.rewriteRuleInlineRolesForRD

main :: IO ()
main = do
    args <- getArgs
    cwd <- getCurrentDirectory
    print args
    print cwd
    case args of
        (path:_) -> do
            ansibleH <- readAnsible (cwd ++ "/" ++ path) "play"
            print "BEGIN A^H ***********************************************************************"
            print ansibleH
            print "END A^H *************************************************************************"
            let ansibleMin = (convertToMin . desugar) ansibleH
            print "END A^H *************************************************************************"
            print ansibleMin
            print "END A^H *************************************************************************"
            let bbfs = runReader (toBBFS ansibleMin) omega
            print "END A^H *************************************************************************"
            print bbfs
            print "END A^H *************************************************************************"
            let idemp = idempotencyCheck ansibleMin bbfs
            print "END A^H *************************************************************************"
            print idemp
            print "END A^H *************************************************************************"
            return ()
        _ -> putStrLn "USAGE: test-executable <path-to-ansible-directory>"
