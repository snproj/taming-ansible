{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE InstanceSigs #-}
module Lexer.YAMLConverter () where

import GHC.Generics

-- import Data.Yaml.Parser (FromYaml (fromYaml))
import Data.Aeson (FromJSON(parseJSON), withObject, (.:), ToJSON (toEncoding), genericToEncoding, defaultOptions, Value (String), Object, toJSON, genericParseJSON)
import Data.Map (Map, lookup)
import Data.Aeson.Key (fromString, Key, toString)
import qualified Data.Text as T
import Data.Aeson.Types (Parser, Value (..), withArray, Object, (.:?), withText, fromJSON, Result (..), Array)
import qualified Data.Map as Map
import qualified Data.HashMap.Strict as HM
import Data.Aeson.KeyMap (toList, lookup, filterWithKey, toList, keys, union, insert, empty)
import Data.List ((\\))
import qualified Data.Vector as V
import qualified Data.Set as Set
import GrammarTypes.AnsibleGrammarTypes
import Data.List.NonEmpty (NonEmpty (..), nonEmpty, singleton, fromList)
import Control.Applicative ((<|>))
import Text.Regex.TDFA ((=~), AllTextMatches (..))
import qualified Data.Maybe
import Debug.Trace (traceShow)
import Data.Scientific (toRealFloat, toBoundedInteger, isInteger)
import Data.Maybe (fromMaybe)
-- import Text.Parsec
-- import Text.Regex.Posix

instance FromJSON Playbook where
    parseJSON = withArray "Playbook" (\arr -> do
        playList <- mapM parseJSON (V.toList arr) :: Parser [Play]
        let nepl = case nonEmpty playList of
                Nothing -> error "ERROR: Playbook has no Plays! Must have at least one!"
                Just pl -> pl
        return (traceShow "" (PlaybookDefinedHere nepl))
        )

instance FromJSON Play where
    parseJSON = withObject "Play" (\obj -> do
        _hostPattern <- obj .: fromString "hosts" :: Parser HostPattern
        _tasks <- obj .:? fromString "tasks" :: Parser (Maybe [TH TaskMarker])
        _handlers <- obj.:? fromString "handlers" :: Parser (Maybe [TH HandlerMarker])
        _roleNames <- obj.:? fromString "roles" :: Parser (Maybe [String])
        (_attSet, _) <- getRemaining obj
        let res = Play {
            hostPattern=_hostPattern,
            playAttributeSet=_attSet,
            tasks=_tasks,
            handlers=_handlers,
            roleNames=_roleNames
        }
        return (traceShow "" res)
        )

instance FromJSON HostPattern where
    parseJSON = withText "HostPattern" (\t -> do
        return ((HostSet . Set.singleton . T.unpack) t) -- TODO: actually parse HostPattern
        )

-- instance FromJSON Role where
--     parseJSON = withObject 
-- instance FromJSON CompulsoryRoleDir
-- instance FromJSON TasksFile
-- instance FromJSON HandlersFile
-- instance FromJSON RoleSubDirFileName

-- instance FromJSON Play where
--     parseJSON = withObject "Play" (\obj -> do
--         _hostPattern <- obj .: fromString "hosts" :: Parser HostPattern
--         _tasks <- obj .:? fromString "tasks" :: Parser (Maybe [Task])
--         _handlers <- obj.:? fromString "handlers" :: Parser (Maybe [Handler])
--         _roles <- obj.:? fromString "roles" :: Parser (Maybe [Role])
--         undefined
--         )
-- jjParseBool :: String -> Parser

jjParse :: String -> [JinjaElem] -- TODO: actually parse JinjaElem
jjParse x = let
    toJJE :: String -> Maybe JinjaElem
    toJJE s
        | length s >= 4 = let
            hasFirstTwoBraces = take 2 s == "{{"
            hasLastTwoBraces = take 2 (reverse s) == "}}"
            in if hasFirstTwoBraces && hasLastTwoBraces
                then let
                    jjContents = take (length s - 4) (drop 2 s) -- remove first and last 2 chars
                    in Just (UnresolvedVarRef jjContents)
                else Just (JustString s)
        | not (null s) = Just (JustString s)
        | otherwise = Nothing
    regex = "([^\\{\\}]*)|\\{\\{([^\\{\\}]*)\\}\\}"
    in Data.Maybe.mapMaybe toJJE (getAllTextMatches (x =~ regex :: AllTextMatches [] String))


-- instance FromJSON JinjaElem where
--     parseJSON = withText "JinjaElem" (\t -> do
--         return (jjParse (T.unpack t))
--         )

checkIfVarContainsJinja :: String -> Bool
checkIfVarContainsJinja s = s =~ "\\{\\{.*\\}\\}" :: Bool

instance FromJSON Var where
    parseJSON v = case v of
        String t -> if checkIfVarContainsJinja (T.unpack t)
                        then withText "VarContainingJinja" (\_ -> do
                                let jje = jjParse (T.unpack t)
                                --jje <- parseJSON v :: Parser [JinjaElem]
                                return (VarContainingJinja jje)
                            ) v
                        else return (SimpleVarString (T.unpack t))
        Array arr -> do
            let valList = V.toList arr
            parsed <- mapM parseJSON valList :: Parser [Var]
            return (ListVar parsed)
        Number n -> return (if isInteger n
                        then case toBoundedInteger n of
                                Nothing -> error ""
                                Just i -> SimpleVarInt i
                        else SimpleVarFloat (toRealFloat n))
        Bool b -> return (SimpleVarBool b)
        Object obj -> do
            let keyList = keys obj
            let m = Map.fromList [(toString k, subVar) |
                    k <- keyList,
                    Just val <- [Data.Aeson.KeyMap.lookup k obj],
                    Success subVar <- [fromJSON val :: Result Var]]
            return (DictVar m)
        _ -> error "ERROR: Value is not a String, Array, Number, Bool or Object!"

instance FromJSON ModDecl

getRemaining :: Object -> Parser (AttributeSet, [Key])
getRemaining obj = do
    let _attSetKM = filterWithKey (\k _ -> k `elem` attSetKeyListGLOBAL) obj
    _attSet  <- parseJSON (toJSON _attSetKM) :: Parser AttributeSet
    let leftoverKeywords = keys obj \\ attSetKeyListGLOBAL
    return (_attSet, leftoverKeywords)

instance FromJSON (TH a) where
    parseJSON :: Value -> Parser (TH a)
    parseJSON = withObject "TH" (\obj -> do
        (_attSet, leftoverKeywords) <- getRemaining obj

        -- let _attSetKM = filterWithKey (\k _ -> k `elem` attSetKeyListGLOBAL) obj
        -- _attSet <- parseJSON (toJSON _attSetKM) :: Parser AttributeSet
        -- let leftoverKeywords = keys obj \\ attSetKeyListGLOBAL

        case leftoverKeywords of
                [] -> error "ERROR: No module or block in TH!"
                other -> if fromString "block" `elem` other
                    then do
                        _block <- obj .: fromString "block"-- :: Parser [TH a]
                        _rescue <- obj .:? fromString "rescue"-- :: Parser (Maybe [TH a])
                        _always <- obj .:? fromString "always"-- :: Parser (Maybe [TH a])
                        return (ContainingBlock _attSet (Block {
                            blockMain=Data.List.NonEmpty.fromList _block,
                            rescue = fmap Data.List.NonEmpty.fromList _rescue,
                            always = fmap Data.List.NonEmpty.fromList _always
                            }) UnsetUID)
                    else do
                        let modName = head other
                        _mod <- obj .: modName :: Parser (Map String Var)
                        let _modDecl = getModDeclVariant (toString modName) _mod
                        return (Atomic _attSet _modDecl UnsetUID)
                -- [modName] -> do
                --     _mod <- obj .: modName :: Parser (Map String Var)
                --     let _modDecl = getModDeclVariant (toString modName) _mod
                --     return (Atomic _attSet _modDecl)
                -- _ -> do
                --     _block <- obj .: fromString "block"-- :: Parser [TH a]
                --     _rescue <- obj .:? fromString "rescue"-- :: Parser (Maybe [TH a])
                --     _always <- obj .:? fromString "always"-- :: Parser (Maybe [TH a])
                --     return (ContainingBlock _attSet (Block {
                --         blockMain=Data.List.NonEmpty.fromList _block,
                --         rescue = fmap Data.List.NonEmpty.fromList _rescue,
                --         always = fmap Data.List.NonEmpty.fromList _always
                --         }))

        -- let res = case toString modNameOrBlockKW of
        --         "block" -> do
        --             _block <- obj .: modNameOrBlockKW :: Parser (Block a)
        --             return (ContainingBlock _attSet _block)
        --         _ -> do
        --             _mod <- obj .: modNameOrBlockKW :: Parser (Map String Var)
        --             let _modDecl = getModDeclVariant (toString modNameOrBlockKW) _mod
        --             return (Atomic _attSet _modDecl)
        -- res
        )
-- requiredDefaultOptionalMSV :: [String] -> Map String Var -> [String] -> Map String Var -> Maybe (Map String Var)
-- requiredDefaultOptionalMSV required defaults optionals msv = undefined



getModDeclVariant :: String -> Map String Var -> ModDecl
getModDeclVariant s msv = case s of
        "import_tasks" -> let
            _file = case Data.Map.lookup "file" msv of
                Just v -> v
                Nothing -> error "ERROR: compulsory keyword not found!"
            in ImportTasks _file
        "include_tasks" -> let
            _file = case Data.Map.lookup "file" msv of
                Just v -> v
                Nothing -> error "ERROR: compulsory keyword not found!"
            _apply = Data.Map.lookup "apply" msv
            in IncludeTasks {file=_file,apply=_apply}
        "import_role" -> let
            _name = case Data.Map.lookup "name" msv of
                Just v -> v
                Nothing -> error ""
            _tasks_from = case Data.Map.lookup "tasks_from" msv of
                Just v -> v
                Nothing -> SimpleVarString "main"
            _handlers_from = case Data.Map.lookup "handlers_from" msv of
                Just v -> v
                Nothing -> SimpleVarString "main"
            in ImportRole {name=_name,tasks_from=_tasks_from,handlers_from=_handlers_from}
        "include_role" -> let
            _name = case Data.Map.lookup "name" msv of
                Just v -> v
                Nothing -> error ""
            _tasks_from = case Data.Map.lookup "tasks_from" msv of
                Just v -> v
                Nothing -> SimpleVarString "main"
            _handlers_from = case Data.Map.lookup "handlers_from" msv of
                Just v -> v
                Nothing -> SimpleVarString "main"
            _apply = Data.Map.lookup "apply" msv
            in IncludeRole {name=_name,tasks_from=_tasks_from,handlers_from=_handlers_from,apply=_apply}
        _ -> GenericModDecl s msv

-- instance FromJSON Handler where
--     parseJSON = withObject "Handler" ( \obj -> do
--         (_attSet, leftoverKeywords) <- getRemaining obj

--         let modNameOrBlockKW = case leftoverKeywords of
--                 [x] -> x
--                 _ -> error "ERROR: After parsing AttributeSet, there was more than one item left. There should be only one, namely the ModDecl"
--         let res = case toString modNameOrBlockKW of
--                 "block" -> do
--                     _block <- obj .: modNameOrBlockKW :: Parser BlockHandler
--                     return (HandlerContainingABlock _attSet _block)
--                 _ -> do
--                     _mod <- obj .: modNameOrBlockKW :: Parser (Map String Var)
--                     let _modDecl = getModDeclVariant (toString modNameOrBlockKW) _mod
--                     return (AtomicHandler _attSet _modDecl)
--         res
--         )


attSetKeyListGLOBAL :: [Key]
attSetKeyListGLOBAL = map fromString ["name", "force_handlers", "notify", "loop", "loop_control", "when", "changed_when", "failed_when", "until", "retries", "register"]
defaultRetriesNumber :: Var
defaultRetriesNumber = SimpleVarInt 3
defaultLoopString :: Var
defaultLoopString = SimpleVarString "item"
defaultWhen :: Var
defaultWhen = SimpleVarBool True
defaultIgnoreErrors :: Var
defaultIgnoreErrors = SimpleVarBool False


-- orDefault :: Maybe a -> a -> Maybe a
-- orDefault maybeThing defaultThing = case maybeThing of
--     Just thing -> Just thing
--     Nothing -> Just defaultThing

instance FromJSON KWLoop where
    parseJSON = withObject "KWLoop" (\obj -> do
        _loopList <- obj .: fromString "loop"
        _loopVar <- obj .:? fromString "loop_var"
        _indexVar <- obj .:? fromString "index_var"
        _pause <- obj .:? fromString "pause"

        -- let _loopVar = fromMaybe defaultLoopString _loopVarByUser

        return (KWLoop {
            loopList=_loopList,
            loopVar=fromMaybe defaultLoopString _loopVar,
            indexVar=_indexVar,
            pause=_pause
        })
        )



instance FromJSON AttributeSet where
    parseJSON = withObject "AttributeSet" (\obj -> do
        _kwName <- obj .:? fromString "name"
        _kwForceHandlers <- obj .:? fromString "force_handlers"
        _kwNotify <- obj .:? fromString "notify"
        _kwWhen <- obj .:? fromString "when"
        _kwVars <- obj .:? fromString "vars"
        _kwChangedWhen <- obj .:? fromString "changed_when"
        _kwFailedWhen <- obj .:? fromString "failed_when"
        _kwUntil <- obj .:? fromString "until"
        _kwRetries <- obj .:? fromString "retries"
        _kwRegister <- obj .:? fromString "register"
        _kwIgnoreErrors <- obj .:? fromString "ignore_errors"

        -- Now the parts that we have to piece together ourselves
        _rawLoopSection <- obj .:? fromString "loop" :: Parser (Maybe Array)
        _rawLoopControlSectionAlreadyDefined <- obj .:? fromString "loop_control" :: Parser (Maybe Object)
        let _kwLoop = do
                _loop <- _rawLoopSection
                let _loopControl = case _rawLoopControlSectionAlreadyDefined of
                        Just _loopControl' -> _loopControl'
                        Nothing -> Data.Maybe.fromMaybe empty _rawLoopControlSectionAlreadyDefined
                let combinedObj = insert (fromString "loop") (Array _loop) _loopControl
                return (case fromJSON (Object combinedObj) :: Result KWLoop of
                        Success a -> a
                        Error err -> error err)

        -- handle the jinjaelems; let's just do this the long way for now
        -- let _kwWhen = do
        --         jjParse <$> _kwWhenString
        -- let _kwChangedWhen = do
        --         jjParse <$> _kwChangedWhenString
        -- let _kwFailedWhen = do
        --         jjParse <$> _kwFailedWhenString
        -- let _kwUntil = do
        --         jjParse <$> _kwUntilString

        return (AttributeSet{
            kwName=_kwName,
            kwForceHandlers = _kwForceHandlers,
            kwNotify = _kwNotify,
            kwLoop = _kwLoop,
            kwWhen = fromMaybe defaultWhen _kwWhen,
            kwVars = _kwVars,
            kwChangedWhen = _kwChangedWhen,
            kwFailedWhen = _kwFailedWhen,
            kwUntil = _kwUntil,
            kwRetries = _kwRetries,
            kwRegister = _kwRegister,
            kwIgnoreErrors = fromMaybe defaultIgnoreErrors _kwIgnoreErrors
            })
        )


-- instance FromJSON (Block a) where
--     parseJSON = withArray "Block" (\arr -> do
--         parsed <- mapM parseJSON (V.toList arr) :: Parser [TH a]
--         let thList = case nonEmpty parsed of
--                 Nothing -> error "ERROR: Block contains no tasks/handlers!"
--                 Just ls -> ls
--         return (Block thList Nothing) -- separate function for adding Rescue, since it's the item containing Block that is in charge of checking that
--         )

-- instance FromJSON (Rescue a) where
--     parseJSON = withArray "Rescue" (\arr -> do
--         parsed <- mapM parseJSON (V.toList arr) :: Parser [TH a]
--         let thList = case nonEmpty parsed of
--                 Nothing -> error "ERROR: Rescue contains no tasks/handlers!"
--                 Just ls -> ls
--         return (Rescue thList Nothing) -- separate function for adding Always, since it's the item containing Block that is in charge of checking that
--         )
-- instance FromJSON (Always a) where
--     parseJSON = withArray "Always" (\arr -> do
--         parsed <- mapM parseJSON (V.toList arr) :: Parser [TH a]
--         let thList = case nonEmpty parsed of
--                 Nothing -> error "ERROR: Always contains no tasks/handlers!"
--                 Just ls -> ls
--         return (Always thList)
--         )
-- instance FromJSON BlockHandler where
--   parseJSON =
--     withArray
--       "BlockHandler"
--       ( \arr -> do
--           parsed <- mapM parseJSON (V.toList arr) :: Parser [Handler]
--           let thList = case nonEmpty parsed of
--                 Nothing -> error "ERROR: BlockHandler contains no handlers!"
--                 Just ls -> ls
--           return (BlockHandler thList Nothing) -- separate function for adding Rescue, since it's the item containing Block that is in charge of checking that
--       )

-- instance FromJSON RescueHandler where
--   parseJSON =
--     withArray
--       "RescueHandler"
--       ( \arr -> do
--           parsed <- mapM parseJSON (V.toList arr) :: Parser [Handler]
--           let thList = case nonEmpty parsed of
--                 Nothing -> error "ERROR: RescueHandler contains no handlers!"
--                 Just ls -> ls
--           return (RescueHandler thList Nothing) -- separate function for adding Always, since it's the item containing Block that is in charge of checking that
--       )

-- instance FromJSON AlwaysHandler where
--   parseJSON =
--     withArray
--       "AlwaysHandler"
--       ( \arr -> do
--           parsed <- mapM parseJSON (V.toList arr) :: Parser [Handler]
--           let thList = case nonEmpty parsed of
--                 Nothing -> error "ERROR: AlwaysHandler contains no handlers!"
--                 Just ls -> ls
--           return (AlwaysHandler thList)
--       )


