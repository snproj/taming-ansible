{-# LANGUAGE InstanceSigs #-}
module Lexer.DirectoryStacker (
    gatherDir,
    AnsibleDir (..),
    AnsibleYAMLFile (..),
    AnsibleFSThing (..)
) where

import Data.Set (Set)
import System.Directory (listDirectory, doesFileExist, doesDirectoryExist)
import System.FilePath (takeExtension, takeBaseName, takeDirectory, (</>))
import Data.Map (fromList, Map)

import Data.Aeson.TH
import qualified Data.Map as Map


data AnsibleFSThing = AFile AnsibleYAMLFile | ADir AnsibleDir deriving (Show)
data AnsibleYAMLFile = AnsibleYAMLFile String
data AnsibleDir = AnsibleDir (Map String AnsibleFSThing)

instance Show AnsibleYAMLFile where
  show :: AnsibleYAMLFile -> String
  show (AnsibleYAMLFile s) = "<file>"-- take 5 s ++ "..."

instance Show AnsibleDir where
  show :: AnsibleDir -> String
  show (AnsibleDir m) = show m

gatherFile :: FilePath -> IO (String, AnsibleYAMLFile)
gatherFile fp = do
    let filename = takeBaseName fp
    fileContents <- readFile fp
    return (filename, AnsibleYAMLFile fileContents)

gatherDir :: FilePath -> IO (String, AnsibleDir)
gatherDir fp = do
    currDirRelative <- listDirectory fp
    let currDir = map (fp </>) currDirRelative
    (files, dirs) <- helper [] [] currDir
    ayfs <- mapM gatherFile files
    adirs <- mapM gatherDir dirs
    let ayfs' = map (\(name, file) -> (name, AFile file)) ayfs
    let adirs' = map (\(name, file) -> (name, ADir file)) adirs
    let combined = ayfs' ++ adirs'
    let combinedMap = Map.fromList combined
    return (takeBaseName fp, AnsibleDir combinedMap)
    where
        helper :: [FilePath] -> [FilePath] -> [FilePath] -> IO ([FilePath], [FilePath])
        helper accFiles accDirs [] = return (accFiles, accDirs)
        helper accFiles accDirs (path : paths) = do
            isFile <- doesFileExist path
            isDir <- doesDirectoryExist path
            print (isFile, isDir)
            case (isFile, isDir) of
                (True, False) -> case takeExtension path of
                    ".json" -> helper (path : accFiles) accDirs paths
                    -- ".yml" -> helper (path : accFiles) accDirs paths
                    -- ".yaml" -> helper (path : accFiles) accDirs paths
                    _ -> helper accFiles accDirs paths -- discard all non-yaml files
                (False, True) -> helper accFiles (path : accDirs) paths
                _ -> error path

