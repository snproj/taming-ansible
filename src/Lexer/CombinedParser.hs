{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
module Lexer.CombinedParser (parseRootDir) where
import Lexer.DirectoryStacker (AnsibleDir (..), AnsibleYAMLFile (..), AnsibleFSThing (..))
import Lexer.YAMLConverter ()
import GrammarTypes.AnsibleGrammarTypes (RootDir (..), Role (..), Task, Handler, Playbook, Play, CompulsoryRoleDir (..), RoleSubDirFileName (..))
import qualified Data.Map as Map
import Data.Aeson.Types (fromJSON, Result, Parser, Value (..))
import Data.Aeson (eitherDecode, decode)
import Data.ByteString.Lazy.Char8 (pack)
import Data.List.NonEmpty (nonEmpty, singleton, NonEmpty (..))
-- import qualified Data.Text
import Debug.Trace (trace, traceShow)

parseRootDir :: String -> AnsibleDir -> RootDir
parseRootDir pbName (AnsibleDir mst) = let
    pbString = case Map.lookup pbName mst of
        Just res -> case res of
                AFile (AnsibleYAMLFile contents) -> contents
                ADir _ -> error "ERROR: Playbook file name actually points to a directory!"
        Nothing -> error "ERROR: Playbook file name not found in root directory!"
    pb = case eitherDecode (pack pbString) :: Either String Playbook of
            Left err -> error err
            Right pb' -> pb'
    _ = traceShow pb
    roleDir = do
        res <- Map.lookup "roles" mst
        case traceShow "fucklala" res of
          AFile _ -> error "ERROR: Role directory is actually a file!"
          ADir (AnsibleDir mst') -> return mst'
    -- roleDirOnlyDirs = do
    --     Map.filter (\athing -> case athing of ADir _ -> True; _ -> False) <$> roleDir
    roles = do
        Map.map (\athing -> case athing of
            ADir adir -> Just (parseRoleFromADir adir)
            _ -> Nothing) <$> roleDir
    roles' = fmap (Map.mapMaybe id) roles
    in traceShow mst (RootDir pb roles')

parseRoleFromADir :: AnsibleDir -> Role
parseRoleFromADir (AnsibleDir mst) = let
    taskDir = Map.lookup "tasks" mst
    taskCRD = do
        parseCRDFromADir "tasks" <$> taskDir

    handlerDir = Map.lookup "handlers" mst
    handlerCRD = do
        parseCRDFromADir "handlers" <$> handlerDir
    
    necrd = case (taskCRD, handlerCRD) of
        (Nothing, Nothing) -> error "ERROR: Neither a tasks dir nor a handlers dir exists in the role! Must have at least one!"
        (Just t, Nothing) -> singleton t
        (Nothing, Just h) -> singleton h
        (Just t, Just h) -> t :| [h]
    in Role necrd



parseCRDFromADir :: String -> AnsibleFSThing -> CompulsoryRoleDir
parseCRDFromADir s (ADir (AnsibleDir adir)) = case s of
    "tasks" -> let
        gotTasks = Map.map (\athing -> case athing of
            ADir _ -> error ""
            AFile afile -> parseTaskListFromAFile afile) adir
        gotRSDFN = Map.mapKeys stringToRSDFN gotTasks
        in TasksDir gotRSDFN
    "handlers" -> let
        gotTasks = Map.map (\athing -> case athing of
            ADir _ -> error ""
            AFile afile -> parseHandlerListFromAFile afile) adir
        gotRSDFN = Map.mapKeys stringToRSDFN gotTasks
        in HandlersDir gotRSDFN
    _ -> error "ERROR: Subdirectory type of Role not supported!"
parseCRDFromADir s (AFile _) = error ("ERROR: " ++ s ++ " must be a directory!")

stringToRSDFN :: String -> RoleSubDirFileName
stringToRSDFN s = case s of
    "main" -> MainName
    -- "main.yaml" -> MainName
    _ -> OtherName s

parseTaskListFromAFile :: AnsibleYAMLFile -> [Task]
parseTaskListFromAFile (AnsibleYAMLFile contents) = case eitherDecode (pack contents) :: Either String [Task] of
        Left s -> error s
        Right taskList -> taskList

parseHandlerListFromAFile :: AnsibleYAMLFile -> [Handler]
parseHandlerListFromAFile (AnsibleYAMLFile contents) = case eitherDecode (pack contents) :: Either String [Handler] of
        Left s -> error s
        Right handlerList -> handlerList