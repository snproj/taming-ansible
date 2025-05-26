{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE InstanceSigs #-}
module Lexer.YAMLConverter () where

import GHC.Generics

-- import Data.Yaml.Parser (FromYaml (fromYaml))
import Data.Aeson (FromJSON(parseJSON), withObject, (.:), ToJSON (toEncoding), genericToEncoding, defaultOptions, Value (String), Object, toJSON, genericParseJSON)
import Data.Map (Map, lookup, empty, fromList)
import Data.Aeson.Key (fromString, Key, toString)
import qualified Data.Text as T
import Data.Aeson.Types (Parser, Value (..), withArray, Object, (.:?), withText, fromJSON, Result (..), Array)
import qualified Data.Map as Map
import qualified Data.HashMap.Strict as HM
import Data.Aeson.KeyMap (toList, lookup, filterWithKey, toList, keys, union, insert, empty, KeyMap)
import Data.List ((\\))
import qualified Data.Vector as V
import qualified Data.Set as Set
import GrammarTypes.AnsibleH
import Data.List.NonEmpty (NonEmpty (..), nonEmpty, singleton, fromList)
import Control.Applicative ((<|>))
import Text.Regex.TDFA ((=~), AllTextMatches (..))
import qualified Data.Maybe
import Debug.Trace (traceShow)
import Data.Scientific (toRealFloat, toBoundedInteger, isInteger)
import Data.Maybe (fromMaybe)
import Semantics.Desugar.DesugarBlocks (createGoalkeeper)

instance FromJSON Play where
    parseJSON = withObject "Play" (\obj -> do
        _tasks <- obj .:? fromString "tasks" :: Parser (Maybe [Task])
        _handlers <- obj.:? fromString "handlers" :: Parser (Maybe [Task])
        _roleNames <- obj.:? fromString "roles" :: Parser (Maybe [String])
        _vars <- obj.:? fromString "vars" :: Parser (Maybe (Map String Var))
        let (_attSetKM, _) = getRemaining obj attSetKeyListGLOBAL
        let res = Play {
            tasks=fromMaybe [] _tasks,
            handlers=fromMaybe [] _handlers,
            roleNames=fromMaybe [] _roleNames,
            vars = fromMaybe Data.Map.empty _vars
        }
        return (traceShow "" res)
        )

wordIsJJ :: String -> Bool
wordIsJJ s = if length s >= 4
    then let
        hasFirstTwoBraces = take 2 s == "{{"
        hasLastTwoBraces = take 2 (reverse s) == "}}"
        in hasFirstTwoBraces && hasLastTwoBraces
    else
        False

getJJContents :: String -> String
getJJContents s = if length s >= 4
    then take (length s - 4) (drop 2 s)
    else error ""

jjParseString :: String -> JinjaPhrase -- TODO: actually parse JinjaElem
jjParseString x = let
    regex = "([^\\{\\}]*)|\\{\\{([^\\{\\}]*)\\}\\}"
    individualWords = getAllTextMatches (x =~ regex :: AllTextMatches [] String)
    in case length individualWords of
        1 | wordIsJJ $ head individualWords -> SingletonUVR $ head individualWords
        _ -> AllEventuallyString $ Data.Maybe.mapMaybe toJSE individualWords
        where
            toJSE :: String -> Maybe JinjaStringElem
            toJSE s
              | null s = Nothing
              | wordIsJJ s = Just (JSE_UVR $ getJJContents s)
              | otherwise = Just (JSE_STRING s)

checkIfVarContainsJinja :: String -> Bool
checkIfVarContainsJinja s = s =~ "\\{\\{.*\\}\\}" :: Bool

instance FromJSON Var where
    parseJSON :: Value -> Parser Var
    parseJSON v = case v of
        String t -> if checkIfVarContainsJinja (T.unpack t)
                        then withText "VarContainingJinja" (\_ -> do
                                let jje = jjParseString (T.unpack t)
                                --jje <- parseJSON v :: Parser [JinjaElem]
                                return (VarContainingJinja jje)
                            ) v
                        else return (SimpleVarString (T.unpack t))
        Bool b -> return (SimpleVarBool b)
        _ -> error "ERROR: Value is not a String, Array, Number, Bool or Object!"

-- instance FromJSON ModDecl

getRemaining :: Object -> [Key] -> (KeyMap Value, [Key])
getRemaining obj attSetKeyList = let
    _attSetKM = filterWithKey (\k _ -> k `elem` attSetKeyList) obj
    leftoverKeywords = keys obj \\ attSetKeyListGLOBAL
    in (_attSetKM, leftoverKeywords)



instance FromJSON Task where
    parseJSON :: Value -> Parser Task
    parseJSON = withObject "TH" (\obj -> do
        let (_attSetKM, leftoverKeywords) = getRemaining obj attSetKeyListGLOBAL

        case leftoverKeywords of
                [] -> error "ERROR: No module or block in TH!"
                other -> if fromString "block" `elem` other
                    then do
                        _block <- obj .: fromString "block"-- :: Parser [TH a]
                        _rescue <- obj .:? fromString "rescue"-- :: Parser (Maybe [TH a])
                        let _rescue' = fromMaybe [] _rescue
                        _always <- obj .:? fromString "always"-- :: Parser (Maybe [TH a])
                        let _always' = fromMaybe [] _always
                        _blockAttSet <- parseJSON (toJSON _attSetKM) :: Parser BlockAttributeSet
                        return (Blocktask _blockAttSet (Block {
                            blockMain= _block,
                            rescue = _rescue',
                            always = _always',
                            goalkeeper = Nothing
                            }) UnsetUID)
                    else do
                        let modName = head other
                        _mod <- obj .:? modName :: (Parser (Maybe (Map String Var)))
                        let _mod' = fromMaybe Data.Map.empty _mod
                        let _modDecl = getModDeclVariant (toString modName) _mod'
                        _atomicAttSet <- parseJSON (toJSON _attSetKM) :: Parser AtomicAttributeSet
                        return (Atomic _atomicAttSet _modDecl UnsetUID)
        )



getModDeclVariant :: String -> Map String Var -> ModDecl
getModDeclVariant s msv = case s of
        "import_tasks" -> let
            _file = case Data.Map.lookup "file" msv of
                Just v -> v
                Nothing -> error "ERROR: compulsory keyword not found!"
            in ImportTasks _file
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
            in ImportRole {name=_name,tasksFrom=_tasks_from,handlersFrom=_handlers_from}
        _ -> GenericModDecl s msv


attSetKeyListGLOBAL :: [Key]
attSetKeyListGLOBAL = map fromString ["name", "force_handlers", "notify", "loop", "loop_control", "when", "changed_when", "failed_when", "until", "retries", "register"]
defaultLoopString :: Var
defaultLoopString = SimpleVarString "item"
defaultWhen :: JBE_EXP
defaultWhen = JBE_EXP_PRIM True
defaultIgnoreErrors :: Bool
defaultIgnoreErrors = False

instance FromJSON KWLoop where
    parseJSON = withObject "KWLoop" (\obj -> do
        _loopList <- obj .: fromString "loop"
        _loopVar <- obj .:? fromString "loop_var"
        _indexVar <- obj .:? fromString "index_var"

        return (KWLoop {
            loopList=_loopList,
            loopVar=fromMaybe defaultLoopString _loopVar,
            indexVar=_indexVar
        })
        )

instance FromJSON AtomicAttributeSet where
    parseJSON = withObject "AtomicAttributeSet" (\obj -> do
        _atomicNotify <- obj .:? fromString "notify"
        _atomicVars <- obj .:? fromString "vars"
        _atomicListen <- obj .:? fromString "listen"

        -- Now the parts that we have to piece together ourselves
        _rawLoopSection <- obj .:? fromString "loop" :: Parser (Maybe Array)
        _rawLoopControlSectionAlreadyDefined <- obj .:? fromString "loop_control" :: Parser (Maybe Object)
        let _atomicLoop = do
                _loop <- _rawLoopSection
                let _loopControl = case _rawLoopControlSectionAlreadyDefined of
                        Just _loopControl' -> _loopControl'
                        Nothing -> Data.Maybe.fromMaybe Data.Aeson.KeyMap.empty _rawLoopControlSectionAlreadyDefined
                let combinedObj = insert (fromString "loop") (Array _loop) _loopControl
                return (case fromJSON (Object combinedObj) :: Result KWLoop of
                        Success a -> a
                        Error err -> error err)

        return (AtomicAttributeSet{
            atomicNotify = fromMaybe [] _atomicNotify,
            atomicLoop = _atomicLoop,
            atomicListen = fromMaybe [] _atomicListen,
            atomicVars = fromMaybe Data.Map.empty _atomicVars,
            atomicWhen = defaultWhen,
            atomicIgnoreErrors = defaultIgnoreErrors
            })
        )

instance FromJSON BlockAttributeSet where
  parseJSON =
    withObject
      "BlockAttributeSet"
      ( \obj -> do
          _blockNotify <- obj .:? fromString "notify"
          _blockVars <- obj .:? fromString "vars"

          return
            ( BlockAttributeSet {
                blockNotify = fromMaybe [] _blockNotify,
                blockVars = fromMaybe Data.Map.empty _blockVars
            })
      )
