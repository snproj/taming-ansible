{-# LANGUAGE LambdaCase #-}
module SymbolicExecution.BBFSSymSem where
import GrammarTypes.BBFS (Expr (..), FS (..))
import SymbolicExecution.Utils (Kappa (..), satTT, toForm, override, overrideKappa, getStartingFS)
import Data.Set
import PropLogic
import qualified Data.Map
import Data.Aeson (Value(Bool))
import Debug.Trace
import GrammarTypes.AnsibleMin

data SymRes = SymErr | SymSuccess (FS, Kappa) deriving (Show, Eq, Ord)

idempotencyCheck :: [Task] -> Expr -> Bool
idempotencyCheck ts program = let
    startingSE = getStartingFS ts
    startingKappa = Kappa T
    firstRun = symbolicSem program (startingSE, startingKappa)
    firstRunWithoutErrs = Data.Set.filter (\case {SymErr->False; _ -> True}) firstRun -- honestly couldve just removed it since its a set lol but whatever
    secondRunChecks = Data.Set.map secondRunIndiv firstRunWithoutErrs
    in and (toList secondRunChecks)
    where
        secondRunIndiv :: SymRes -> Bool
        secondRunIndiv SymErr = error "ERROR: tried doing second run on a SymErr!"
        secondRunIndiv (SymSuccess (fs, k)) = let
            res = symbolicSem program (fs,k)
            in case toList res of
                [r] -> r /= SymErr
                _ -> False

symbolicSem :: Expr -> (FS, Kappa) -> Set SymRes
symbolicSem expr (fs, k) = case expr of
    Err -> singleton SymErr
    Seq e1 e2 -> let
        e1Reses = symbolicSem e1 (fs,k)
        setOfE2Reses = Data.Set.map (\case {SymErr -> singleton SymErr; SymSuccess (fs', k') -> symbolicSem e2 (fs', k')}) e1Reses
        in Data.Set.unions setOfE2Reses
    Ask q ifBranch elseBranch -> let
        Kappa kform = k
        Kappa qform = toForm q
        
        st = satTT (Kappa (conj [kform, qform]))
        -- bleh = conj [kform, qform]
        -- st = traceShow bleh $ satTT (Kappa (bleh))
        stForms = Prelude.map ((\(Kappa x) -> x) . toForm) st
        kt = Kappa (conj [kform, disj stForms])

        sf = satTT (Kappa (conj [kform, N qform]))
        sfForms = Prelude.map ((\(Kappa x) -> x) . toForm) sf
        kf = Kappa (conj [kform, disj sfForms])
        in case (st, sf) of
            ([],[]) -> error "ERROR: ask reached impossible scenario?"
            (_,[]) -> symbolicSem ifBranch (fs, kt)
            ([],_) -> symbolicSem elseBranch (fs, kf)
            (_,_) -> Data.Set.union (symbolicSem ifBranch (fs, kt)) (symbolicSem elseBranch (fs, kf))
    Trans t -> let
        fs' = override t fs
        k' = overrideKappa t k
        in fromList [SymSuccess (fs', k')]

