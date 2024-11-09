module Main (main) where

import Lexer.YAMLConverter
import qualified Data.ByteString.Lazy as B
import GrammarTypes.AnsibleGrammarTypes
import Data.Aeson
import Lexer.DirectoryStacker
import Lexer.CombinedParser
import Semantics.StaticVarResolver
import Debug.Trace (trace, traceShow)
import Control.Monad.Reader (runReader)
import Data.Map (fromList, Map)
import Semantics.Desugar.DesugarLoops
import Semantics.Desugar.DesugarRoles
import Data.List.NonEmpty (head)

testYaml :: IO ()
testYaml = do
    json <- B.readFile "/home/sunrise/research/test2.json"
    let res = eitherDecode json :: Either String (TH TaskMarker)
    case res of
        Left s -> print s
        Right t -> print t
    return ()

testSVR :: IO ()
testSVR = do
    json <- B.readFile "/home/sunrise/research/testSVR.json"
    let res = eitherDecode json :: Either String (TH TaskMarker)
    let res' = case res of
            Left s -> error ""
            Right t -> runReader (resolveTH t) (SymbolTable (fromList [
                ("var1", SimpleVarString "resolved1"),
                ("var2", SimpleVarString "resolved2"),
                ("var3", SimpleVarString "resolved3")
                ]))
    print res'
    return ()

testDSPlainRole :: IO ()
testDSPlainRole = do
    (name, dir) <- gatherDir "/home/sunrise/research/testansibleproj"
    let root = parseRootDir "play" dir
    let (p, mmsr) = getPBMMSR root
    let res = runReader (desugarPlainRoleCalls p) mmsr
    print res
    return ()
        where
            getPBMMSR :: RootDir -> (Play, Maybe (Map String Role))
            getPBMMSR (RootDir (PlaybookDefinedHere nePlay) mmsr) = (Data.List.NonEmpty.head nePlay, mmsr)


testUnrollLoopForGenericMod :: IO ()
testUnrollLoopForGenericMod = do
    json <- B.readFile "/home/sunrise/research/taskGenericModDeclWithLoop.json"
    let res = eitherDecode json :: Either String (TH TaskMarker)
    let res' = case res of
            Left s -> error "ERROR: Some error during parsing the task!"
            Right t -> case t of
                (Atomic attSet modDecl) -> let
                    _kwLoop = case kwLoop attSet of
                        Nothing -> error ""
                        Just _kwLoop' -> _kwLoop'
                    basic = unrollLoopBasic _kwLoop modDecl
                    in runReader (resolveUnrolledModDeclWithLoopVar basic) _kwLoop
                _ -> error ""
    print res'
    return ()


testDirStacker :: IO ()
testDirStacker = do
    (name, dir) <- gatherDir "/home/sunrise/antsy/demo1/"
    print name
    print dir


testParseRoot :: IO ()
testParseRoot = do
    (name, dir) <- gatherDir "/home/sunrise/research/testansibleproj"
    print name
    print dir
    let root = parseRootDir "play" dir
    print root
    return ()



main :: IO ()
main = do
    -- print "what"
    -- let s = traceShow "fucky boi" "l"
    -- print s
    testParseRoot
    -- testDirStacker
    -- testDSPlainRole
    -- testSVR
    -- testYaml
    -- testUnrollLoopForGenericMod
    return ()
