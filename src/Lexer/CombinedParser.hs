{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# LANGUAGE LambdaCase #-}
module Lexer.CombinedParser (parseRootDir) where
import Lexer.DirectoryStacker (AnsibleDir (..), AnsibleYAMLFile (..), AnsibleFSThing (..))
import Lexer.YAMLConverter ()
import GrammarTypes.AnsibleGrammarTypes --(RootDir (..), Role (..), Playbook, Play, CompulsoryRoleDir (..), RoleSubDirFileName (..))
import qualified Data.Map as Map
import Data.Aeson.Types (fromJSON, Result, Parser, Value (..))
import Data.Aeson (eitherDecode, decode)
import Data.ByteString.Lazy.Char8 (pack)
import Data.List.NonEmpty (nonEmpty, singleton, NonEmpty (..))
-- import qualified Data.Text
import Debug.Trace (trace, traceShow)
import Data.Map (Map)

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
    -- _ = traceShow pb
    roleDir = do
        res <- Map.lookup "roles" mst
        case res of
          AFile _ -> error "ERROR: Role directory is actually a file!"
          ADir (AnsibleDir mst') -> return mst'
    -- roleDirOnlyDirs = do
    --     Map.filter (\athing -> case athing of ADir _ -> True; _ -> False) <$> roleDir
    roles = do
        Map.map (\athing -> case athing of
            ADir adir -> Just (parseRoleFromADir adir)
            _ -> Nothing) <$> roleDir
    roles' = fmap (Map.mapMaybe id) roles
    _looseTaskFiles = do
        let onlyFiles = Map.filter (\case {ADir _ -> False; AFile _ -> True}) mst
        let withoutPB = Map.filterWithKey (\k _ -> k /= pbName) onlyFiles -- do not read the playbook file as a loose task file!
        let convToAYF = Map.map (\(AFile a) -> a) withoutPB
        return (Map.map parseTaskListFromAFile convToAYF) :: Maybe (Map String [TH TaskMarker])
    in RootDir {
        playbook=pb,
        roledir=roles',
        looseTaskFiles=_looseTaskFiles   
        }

parseRoleFromADir :: AnsibleDir -> Role
parseRoleFromADir (AnsibleDir mst) = let
    taskDir = Map.lookup "tasks" mst
    taskCRD = do
        parseTasksDir <$> taskDir

    handlerDir = Map.lookup "handlers" mst
    handlerCRD = do
        parseHandlersDir <$> handlerDir
    
    -- necrd = case (taskCRD, handlerCRD) of
    --     (Nothing, Nothing) -> error "ERROR: Neither a tasks dir nor a handlers dir exists in the role! Must have at least one!"
    --     (Just t, Nothing) -> singleton t
    --     (Nothing, Just h) -> singleton h
    --     (Just t, Just h) -> t :| [h]
    in Role {
        tasksDir=taskCRD,
        handlersDir=handlerCRD
    }


parseTasksDir :: AnsibleFSThing -> Map RoleSubDirFileName [TH TaskMarker]
parseTasksDir (ADir (AnsibleDir adir)) = let
    gotTasks = Map.map (\athing -> case athing of
        ADir _ -> error ""
        AFile afile -> parseTaskListFromAFile afile) adir
    gotRSDFN = Map.mapKeys stringToRSDFN gotTasks
    in gotRSDFN

parseHandlersDir :: AnsibleFSThing -> Map RoleSubDirFileName [TH HandlerMarker]
parseHandlersDir (ADir (AnsibleDir adir)) =
  let gotHandlers =
        Map.map
          ( \athing -> case athing of
              ADir _ -> error ""
              AFile afile -> parseHandlerListFromAFile afile
          )
          adir
      gotRSDFN = Map.mapKeys stringToRSDFN gotHandlers
   in gotRSDFN

-- parseCRDFromADir :: String -> AnsibleFSThing -> Maybe (Map RoleSubDirFileName [TH TaskMarker])
-- parseCRDFromADir s (ADir (AnsibleDir adir)) = case s of
--     "tasks" -> let
--         gotTasks = Map.map (\athing -> case athing of
--             ADir _ -> error ""
--             AFile afile -> parseTaskListFromAFile afile) adir
--         gotRSDFN = Map.mapKeys stringToRSDFN gotTasks
--         in gotRSDFN
--     "handlers" -> let
--         gotTasks = Map.map (\athing -> case athing of
--             ADir _ -> error ""
--             AFile afile -> parseHandlerListFromAFile afile) adir
--         gotRSDFN = Map.mapKeys stringToRSDFN gotTasks
--         in HandlersDir gotRSDFN
--     _ -> error "ERROR: Subdirectory type of Role not supported!"
-- parseCRDFromADir s (AFile _) = error ("ERROR: " ++ s ++ " must be a directory!")

stringToRSDFN :: String -> RoleSubDirFileName
stringToRSDFN s = case s of
    "main" -> MainName
    -- "main.yaml" -> MainName
    _ -> OtherName s

parseTaskListFromAFile :: AnsibleYAMLFile -> [TH TaskMarker]
parseTaskListFromAFile (AnsibleYAMLFile contents) = case eitherDecode (pack contents) :: Either String [TH TaskMarker] of
        Left s -> error s
        Right taskList -> taskList

parseHandlerListFromAFile :: AnsibleYAMLFile -> [TH HandlerMarker]
parseHandlerListFromAFile (AnsibleYAMLFile contents) = case eitherDecode (pack contents) :: Either String [TH HandlerMarker] of
        Left s -> error s
        Right handlerList -> handlerList