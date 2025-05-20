module GrammarTypes.HToMin
    (
        
    ) where
import GrammarTypes.AnsibleH (RootDir (playbook), tasks, Task (..), Play(..), AtomicAttributeSet (..), JBE_EXP (..), UID (..), JBE_TEST (..), JBE_BINOP (..), ModDecl (..), mustBeString, Var (..))
import GrammarTypes.AnsibleMin (Task (..), AttributeSet (..), JBE_EXP(..), JBE_TEST (JBE_TEST_DEFINED, JBE_TEST_SUCCEEDED, JBE_TEST_FAILED), JBE_BINOP (JBE_OP_AND, JBE_OP_OR), ModDecl (..), Var (SimpleVarString, SimpleVarBool))
import qualified Data.Map

convertToMin :: RootDir -> [GrammarTypes.AnsibleMin.Task]
convertToMin rd = map htask2mintask $ concatMap tasks (playbook rd)

htask2mintask :: GrammarTypes.AnsibleH.Task -> GrammarTypes.AnsibleMin.Task
htask2mintask ht = case ht of
    Atomic {} -> Task {
        attributeSet = aas2as (atomicAttributeSet ht),
        GrammarTypes.AnsibleMin.modDecl = hmd2minmd (GrammarTypes.AnsibleH.modDecl ht),
        GrammarTypes.AnsibleMin.uid = huid2minuid (GrammarTypes.AnsibleH.uid ht)
    }
    _ -> error "ERROR: All tasks should be atomic at this point!"

huid2minuid :: GrammarTypes.AnsibleH.UID -> String
huid2minuid hu = case hu of
    UnsetUID -> error "ERROR: all UIDs should have been set by now!"
    SetUID s -> s

hmd2minmd :: GrammarTypes.AnsibleH.ModDecl -> GrammarTypes.AnsibleMin.ModDecl
hmd2minmd hmd = case hmd of
    GenericModDecl n p -> ModDecl {
        GrammarTypes.AnsibleMin.name = n,
        GrammarTypes.AnsibleMin.params = Data.Map.map hvar2minvar p
    }
    _ -> error "ERROR: all tasks should be genericmoddecl by this point!"

hvar2minvar :: GrammarTypes.AnsibleH.Var -> GrammarTypes.AnsibleMin.Var
hvar2minvar hvar = case hvar of
    GrammarTypes.AnsibleH.SimpleVarString s -> GrammarTypes.AnsibleMin.SimpleVarString s
    GrammarTypes.AnsibleH.SimpleVarBool b -> GrammarTypes.AnsibleMin.SimpleVarBool b
    GrammarTypes.AnsibleH.VarContainingJinja _ -> error "ERROR: vars should all be resolved by now!" 

aas2as :: AtomicAttributeSet -> AttributeSet
aas2as aas = AttributeSet {when=hjbe2minjbe (atomicWhen aas), ignoreErrors=atomicIgnoreErrors aas}

hjbe2minjbe :: GrammarTypes.AnsibleH.JBE_EXP -> GrammarTypes.AnsibleMin.JBE_EXP
hjbe2minjbe hj = case hj of
    GrammarTypes.AnsibleH.JBE_EXP_REGTEST (SetUID s) test -> GrammarTypes.AnsibleMin.JBE_EXP_REGTEST s (htest2mintest test)
    GrammarTypes.AnsibleH.JBE_EXP_REGTEST UnsetUID _ -> error "ERROR: all uids should be set by now!"
    GrammarTypes.AnsibleH.JBE_EXP_BINARYOP e1 op e2 -> GrammarTypes.AnsibleMin.JBE_EXP_BINARYOP (hjbe2minjbe e1) (hop2minop op) (hjbe2minjbe e2)
    GrammarTypes.AnsibleH.JBE_EXP_NOT e -> GrammarTypes.AnsibleMin.JBE_EXP_NOT (hjbe2minjbe e)
    GrammarTypes.AnsibleH.JBE_EXP_PARENEXP e -> GrammarTypes.AnsibleMin.JBE_EXP_PARENEXP (hjbe2minjbe e)
    GrammarTypes.AnsibleH.JBE_EXP_PRIM b -> GrammarTypes.AnsibleMin.JBE_EXP_PRIM b

htest2mintest :: GrammarTypes.AnsibleH.JBE_TEST -> GrammarTypes.AnsibleMin.JBE_TEST
htest2mintest ht = case ht of
    GrammarTypes.AnsibleH.JBE_TEST_DEFINED -> GrammarTypes.AnsibleMin.JBE_TEST_DEFINED
    GrammarTypes.AnsibleH.JBE_TEST_SUCCEEDED -> GrammarTypes.AnsibleMin.JBE_TEST_SUCCEEDED
    GrammarTypes.AnsibleH.JBE_TEST_FAILED -> GrammarTypes.AnsibleMin.JBE_TEST_FAILED

hop2minop :: GrammarTypes.AnsibleH.JBE_BINOP -> GrammarTypes.AnsibleMin.JBE_BINOP
hop2minop hb = case hb of
    GrammarTypes.AnsibleH.JBE_OP_AND -> GrammarTypes.AnsibleMin.JBE_OP_AND
    GrammarTypes.AnsibleH.JBE_OP_OR -> GrammarTypes.AnsibleMin.JBE_OP_OR



