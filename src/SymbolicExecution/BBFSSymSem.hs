{-# LANGUAGE LambdaCase #-}
module SymbolicExecution.BBFSSymSem where
import GrammarTypes.BBFS (Expr (..), FS (..))
import SymbolicExecution.Utils (Kappa (..), satTT, toForm)
import Data.Set
import PropLogic

data SymRes = SymErr | SymSuccess (FS, Kappa) deriving (Show, Eq, Ord)

idempotencyCheck :: Expr -> Bool
idempotencyCheck = undefined

-- firstRun :: Expr -> Set SymRes
-- firstRun = undefined

-- secondRun :: 

symbolicSem :: Expr -> (FS, Kappa) -> Set SymRes
symbolicSem expr (fs, k) = case expr of
    Err -> empty
    Seq e1 e2 -> let
        e1Reses = symbolicSem e1 (fs,k)
        setOfE2Reses = Data.Set.map (\case {SymErr -> singleton SymErr; SymSuccess (fs', k') -> symbolicSem e2 (fs', k')}) e1Reses
        in Data.Set.unions setOfE2Reses
    Ask q ifBranch elseBranch -> let
        Kappa kform = k
        Kappa qform = toForm q

        st = satTT (Kappa (conj [kform, qform]))
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
    Trans (FS mpb) -> let
        
        in undefined

