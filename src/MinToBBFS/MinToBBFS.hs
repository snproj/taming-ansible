{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
module MinToBBFS.MinToBBFS where
import GrammarTypes.BBFS (Expr (..), Path (..), FS (..))
import GrammarTypes.AnsibleMin (Task (..), ModDecl (..), Var (..))
import TBBFSToBBFS.TBBFSToBBFS (Delta (..), Template (..))
import Data.Map
import qualified GrammarTypes.TBBFS
import Control.Monad.Reader
import GrammarTypes.BBFSInfra (successRoot, mR, mS)
import MinToBBFS.WhenTransformation (whenTransformation)
import MinToBBFS.IgnoreErrorTransformation (ignoreErrorTransformation)

-- [Task] -> BBFS

-- Task -> BBFS



writeAbstractPathTBBFS :: String -> Bool -> String -> GrammarTypes.TBBFS.Expr
writeAbstractPathTBBFS abstractRoot val u = GrammarTypes.TBBFS.Trans (GrammarTypes.TBBFS.FS (fromList [(
    GrammarTypes.TBBFS.TemplatablePath [
        GrammarTypes.TBBFS.LitString abstractRoot,
        GrammarTypes.TBBFS.LitString u],
    GrammarTypes.TBBFS.LitBool val)]))



mSTBBFS :: String -> GrammarTypes.TBBFS.Expr
mSTBBFS = writeAbstractPathTBBFS successRoot True

newtype Omega = Omega {mse :: Map String GrammarTypes.TBBFS.Expr}

writeLoopGoalkeeper :: String -> Delta -> GrammarTypes.TBBFS.Expr
writeLoopGoalkeeper u delta = let
    l = Prelude.map fst $ toList $ mss delta
    in buildChain l
    where
        buildChain :: [String] -> GrammarTypes.TBBFS.Expr
        buildChain l = case l of
            [] -> error "ERROR: should not happen!"
            [single] -> GrammarTypes.TBBFS.Ask
                (GrammarTypes.TBBFS.FS (fromList [(
                    GrammarTypes.TBBFS.TemplatablePath [GrammarTypes.TBBFS.LitString single],
                    GrammarTypes.TBBFS.LitBool True)]))
                (mSTBBFS u)
                GrammarTypes.TBBFS.Err
            s:ss -> GrammarTypes.TBBFS.Ask
                (GrammarTypes.TBBFS.FS (fromList [(
                    GrammarTypes.TBBFS.TemplatablePath [GrammarTypes.TBBFS.LitString s],
                    GrammarTypes.TBBFS.LitBool True)]))
                (buildChain ss)
                GrammarTypes.TBBFS.Err

getModDeclTBBFS :: Task -> Reader Omega GrammarTypes.TBBFS.Expr
getModDeclTBBFS t = case name (modDecl t) of
    "_lgk" -> return $ writeLoopGoalkeeper (uid t) (paramsToDelta (params (modDecl t)))
    n -> do
        omega <- ask
        case Data.Map.lookup n (mse omega) of
            Nothing -> error "ERROR: lookup failed for tbbfs during translation!"
            Just texpr' -> return texpr'

-- getModDeclTBBFS :: String -> Delta -> Omega -> ModDecl -> GrammarTypes.TBBFS.Expr
-- getModDeclTBBFS u delta omega md = case name md of
--     "_lgk" -> writeLoopGoalkeeper u delta
--     n -> case Data.Map.lookup n (mse omega) of
--         Nothing -> error "ERROR: lookup failed for tbbfs during translation!"
--         Just texpr' -> texpr'

paramsToDelta :: Map String Var -> Delta
paramsToDelta msv = let
    isBoolEntry = \case {SimpleVarBool _ -> True; SimpleVarString _ -> False}
    boolEntries = Data.Map.filter isBoolEntry msv
    boolEntries' = Data.Map.map (\case {SimpleVarBool b -> b}) boolEntries
    stringEntries = Data.Map.filter (not . isBoolEntry) msv
    stringEntries' = Data.Map.map (\case {SimpleVarString s -> s}) stringEntries
    in Delta {
        msb = boolEntries',
        mss = stringEntries'
    }

class Translatable a where
    toBBFS :: a -> Reader Omega Expr

-- instance Translatable ModDecl where
--     toBBFS :: ModDecl -> Reader Omega Expr
--     toBBFS md = do
--         let delta = paramsToDelta (params md)
--         omega <- ask
--         let texpr = getModDeclTBBFS delta omega md
--         let expr = runReader (resolve texpr) delta
--         return expr

bracketWithmSmR :: String -> Expr -> Expr
bracketWithmSmR u expr = Seq (mS u) (Seq expr (mR u))

instance Translatable Task where
    toBBFS :: Task -> Reader Omega Expr
    toBBFS t = do
        let md = modDecl t
        let delta = paramsToDelta (params md)
        texpr <- getModDeclTBBFS t
        let expr = runReader (resolve texpr) delta
        let bracketedExpr = bracketWithmSmR (uid t) expr
        let whenExpr = whenTransformation t bracketedExpr
        let ignoreErrorExpr = ignoreErrorTransformation t whenExpr
        return ignoreErrorExpr