{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module TBBFSToBBFS.TBBFSToBBFS where
import Control.Monad.Reader
import Data.Map
import GrammarTypes.TBBFS (TemplatablePath (..), PathPart(..), TemplatableBool (..), FS (..), Expr(..))
import GrammarTypes.BBFS (Path(..), FS(..), Expr(..))
import Data.List.Split

data Delta = Delta {
    mss :: Map String String,
    msb :: Map String Bool
}

class Template a b where
    resolve :: a -> Reader Delta b

instance Template TemplatableBool Bool where
    resolve :: TemplatableBool -> Reader Delta Bool
    resolve tb = case tb of
        LitBool b -> return b
        TSubB s -> do
            d <- ask
            case Data.Map.lookup s (msb d) of
                Nothing -> error "ERROR: no mapping found to resolve TSubB!"
                Just b' -> return b'

instance Template PathPart [String] where
    resolve :: PathPart -> Reader Delta [String]
    resolve pp = case pp of
        LitString s -> if '\\' `elem` s then
                error "ERROR: slash contained within LitString!"
            else
                return [s]
        TSubP s -> do
            d <- ask
            case Data.Map.lookup s (mss d) of
                Nothing -> error "ERROR: no mapping found to resolve TSubP!"
                Just s' -> return (splitOn "\\" s') -- TODO: sort out how to treat multi-part paths

instance Template TemplatablePath Path where
    resolve :: TemplatablePath -> Reader Delta Path
    resolve (TemplatablePath pps) = do
        res <- traverse resolve pps
        let cres = concat res
        return $ Path cres

instance Template GrammarTypes.TBBFS.FS GrammarTypes.BBFS.FS where
    resolve :: GrammarTypes.TBBFS.FS -> Reader Delta GrammarTypes.BBFS.FS
    resolve (GrammarTypes.TBBFS.FS tfs) = do
        let tfsList = Data.Map.toList tfs
        res <- traverse resolveEntry tfsList
        return $ GrammarTypes.BBFS.FS (fromList res)
        where
            resolveEntry :: (TemplatablePath, TemplatableBool) -> Reader Delta (Path, Bool)
            resolveEntry (k, v) = do
                rk <- resolve k
                rv <- resolve v
                pure (rk, rv)

instance Template GrammarTypes.TBBFS.Expr GrammarTypes.BBFS.Expr where
    resolve :: GrammarTypes.TBBFS.Expr -> Reader Delta GrammarTypes.BBFS.Expr
    resolve texpr = case texpr of
        GrammarTypes.TBBFS.Err -> return GrammarTypes.BBFS.Err
        GrammarTypes.TBBFS.Ask fs ex1 ex2 -> do
            fs' <- resolve fs
            ex1' <- resolve ex1
            ex2' <- resolve ex2
            return $ GrammarTypes.BBFS.Ask fs' ex1' ex2'
        GrammarTypes.TBBFS.Seq ex1 ex2 -> do
            ex1' <- resolve ex1
            ex2' <- resolve ex2
            return $ GrammarTypes.BBFS.Seq ex1' ex2'
        GrammarTypes.TBBFS.Trans fs -> do
            fs' <- resolve fs
            return $ GrammarTypes.BBFS.Trans fs'
        GrammarTypes.TBBFS.TChoice s mse -> do
            d <- ask
            case Data.Map.lookup s (mss d) of
                Nothing -> error "ERROR: no mapping found to resolve TChoice!"
                Just s' -> case Data.Map.lookup s' mse of
                    Nothing -> error "ERROR: no local mapping found within TChoice!"
                    Just ex -> do
                        resolve ex


-- resolve :: GrammarTypes.TBBFS.Expr -> Reader Delta GrammarTypes.BBFS.Expr
-- resolve texpr = do
--     d <- ask

--     undefined