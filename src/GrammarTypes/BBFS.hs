module GrammarTypes.BBFS (
    Path(..),
    FS(..),
    Expr(..),
    updateFS,
    checkIntegrity,
) where
import Data.Map

newtype Path = Path [String] deriving (Show, Eq, Ord)

getParent :: Path -> Path
getParent (Path []) = Path []
getParent (Path ss) = Path (init ss)

getParentChain :: Path -> [Path]
getParentChain (Path []) = [Path []]
getParentChain (Path ss) = let
    parent = getParent (Path ss)
    in parent : getParentChain parent

isAncestorOf :: Path -> Path -> Bool
isAncestorOf p1 p2 = p1 `elem` getParentChain p2

newtype FS = FS (Map Path Bool) deriving (Show, Eq, Ord)

-- TODO: optimize if path already present
updatePathPresent :: FS -> Path -> FS
updatePathPresent (FS mpb) pathToAdd = let
    mpb' = insert pathToAdd True mpb
    in updatePathPresent (FS mpb') (getParent pathToAdd)

updatePathAbsent :: FS -> Path -> FS
updatePathAbsent (FS mpb) pathToDelete = let
    mpb' = Data.Map.filterWithKey (\p b -> not (b && pathToDelete `isAncestorOf` p)) mpb
    mpb'' = delete pathToDelete mpb'
    in FS mpb''

updateFS :: Bool -> FS -> Path -> FS
updateFS b fs p = if b then
        updatePathPresent fs p
    else
        updatePathAbsent fs p

checkIntegrity :: FS -> Bool
checkIntegrity fs = let
    FS mpb = fs
    pbList = toList mpb
    emptyFS = FS empty
    fs' = Prelude.foldl (\efs (p,b) -> updateFS b efs p) emptyFS pbList
    in fs == fs'



data Expr
    = Err
    | Ask FS Expr Expr
    | Seq Expr Expr
    | Trans FS
    deriving (Show, Eq, Ord)
