module SymbolicExecution.Utils where

import PropLogic
import GrammarTypes.BBFS (Path, FS (..), updateFS, checkIntegrity, fleshOut)
import Olist
import Data.Map
import Debug.Trace (trace, traceShow)

fsToVal :: FS -> Valuator Path
fsToVal (FS mpb) = olist (toList mpb)

kComb :: Kappa -> FS -> Bool
kComb (Kappa form) fs = let
    valuator = fsToVal fs
    in boolEval $ valuate valuator form



newtype Kappa = Kappa (PropForm Path) deriving (Show, Eq, Ord)

satTT :: Kappa -> [FS]
satTT (Kappa pf) = let
    tt = truthTable pf
    (v, _, ttl) = tt
    satTTList = Prelude.map fst (Prelude.filter snd ttl)
    -- We can't use `updateFS` to assemble FSes directly because
    -- ironically this forces the FSes to be sound, but they might
    -- not be the ones we started with in the truthtable
    possiblyUnsoundMaps = Prelude.map (assembleMap v) satTTList
    possiblyUnsoundFSes = Prelude.map FS possiblyUnsoundMaps
    soundFSes = traceShow possiblyUnsoundFSes $ Prelude.filter checkIntegrity possiblyUnsoundFSes
    fleshedOutFSes = traceShow soundFSes $ Prelude.map fleshOut soundFSes
    in traceShow fleshedOutFSes $ fleshedOutFSes
    where
        assembleMap :: Olist Path -> [Bool] -> Map Path Bool
        assembleMap [] [] = Data.Map.empty
        assembleMap (v:vs) (b:bs) = Data.Map.insert v b (assembleMap vs bs)
        assembleMap _ _ = error "ERROR: Olist and truth table vars don't seem to match in length?"

toForm :: FS -> Kappa
toForm (FS mpb) = let
    mpbList = toList mpb
    as = Prelude.map (\(p,b) -> if b then A p else N (A p)) mpbList
    anded = conj as
    in Kappa anded

override :: FS -> FS -> FS
override (FS mpb1) (FS mpb2) = FS (Data.Map.union mpb1 mpb2)

overrideKappa :: FS -> Kappa -> Kappa
overrideKappa fs k = let
    satFSs = satTT k
    overriddenSatFSs = Prelude.map (override fs) satFSs
    forms = Prelude.map toForm overriddenSatFSs
    rawForms = Prelude.map (\(Kappa f)->f) forms
    in Kappa $ disj rawForms