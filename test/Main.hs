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
import Data.Map (fromList, Map, empty)
import Semantics.Desugar.DesugarLoops
import Semantics.Desugar.DesugarRoles
import Data.List.NonEmpty (head, map)
import qualified Text.Regex.TDFA.CorePattern as Data.List
import Semantics.Desugar.DesugarImports (desugarImportsInPlay)
import Semantics.UIDSetter
import Semantics.Desugar.DesugarBlocks (chainTogether, drawArrowsWithinBlock)
import Data.Maybe (fromJust)
import Semantics.RegSetter
import Semantics.Desugar.DesugarBlocks
import Semantics.StaticVarResolver (resolveContainedUVRs)
import Semantics.Desugar.DesugarHandlers (desugarHandlersInPlay)

testYaml :: IO ()
testYaml = do
    json <- B.readFile "/home/sunrise/research/test2.json"
    let res = eitherDecode json :: Either String Task
    case res of
        Left s -> print s
        Right t -> print t
    return ()

testSVR :: IO ()
testSVR = do
    json <- B.readFile "/home/sunrise/research/testSVR.json"
    let res = eitherDecode json :: Either String Task
    let res' = case res of
            Left s -> error ""
            Right t -> runReader (resolveContainedUVRs t) (SymbolTable (fromList [
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
    -- let (p, mmsr) = getPBMMSR root
    let nep = playbook root
    let p = Data.List.NonEmpty.head nep
    let mmsr = roledir root
    let res = runReader (desugarPlainRoleCalls p) mmsr
    print res
    return ()
        -- where
        --     getPBMMSR :: RootDir -> (Play, Maybe (Map String Role))
        --     getPBMMSR (RootDir (PlaybookDefinedHere nePlay) mmsr) = (Data.List.NonEmpty.head nePlay, mmsr)


testUnrollLoopForGenericMod :: IO ()
testUnrollLoopForGenericMod = do
    json <- B.readFile "/home/sunrise/research/taskGenericModDeclWithLoop.json"
    let res = eitherDecode json :: Either String Task
    let res' = case res of
            Left s -> error "ERROR: Some error during parsing the task!"
            Right t -> case t of
                (Atomic attSet modDecl _) -> let
                    _kwLoop = case atomicLoop attSet of
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

testDesugarImports :: IO ()
testDesugarImports = do
    (name, dir) <- gatherDir "/home/sunrise/research/testansibleproj"
    print name
    print dir
    let root = parseRootDir "play" dir
    let nep = playbook root
    let play = Data.List.NonEmpty.head nep
    let res = runReader (desugarImportsInPlay play) root
    print res
    return ()

testUIDSetting :: IO ()
testUIDSetting = do
    (name, dir) <- gatherDir "/home/sunrise/research/testansibleproj"
    let root = parseRootDir "play" dir
    let nep = playbook root
    let play = Data.List.NonEmpty.head nep
    let res = runReader (setUID play) (SetUID "")
    print res
    return ()

testDrawArrows :: IO ()
testDrawArrows = do
    (name, dir) <- gatherDir "/home/sunrise/research/testansibleproj"
    let root = parseRootDir "play" dir
    let nep = playbook root
    let play = Data.List.NonEmpty.head nep
    let playWithUIDs = runReader (setUID play) (SetUID "")
    let playWithRegs = setReg playWithUIDs
    let tasksWithRegs = tasks playWithRegs
    let b = tasksWithRegs !! 3
    let res = flattenBlocks $ chainTogether tasksWithRegs
    print res
    return ()

testWholeShebang :: String -> String -> IO ()
testWholeShebang dirname playfilename = do
    (name, dir) <- gatherDir dirname
    let rd = parseRootDir playfilename dir
    let pb = playbook rd
    let pb' = Data.List.NonEmpty.map (\p -> runReader (desugarPlainRoleCalls p) (roledir rd)) pb
    let pb'' = Data.List.NonEmpty.map (\p -> runReader (desugarImportsInPlay p) rd) pb'
    let pb''' = Data.List.NonEmpty.map (\p -> runReader (resolveContainedUVRs p) (SymbolTable Data.Map.empty)) pb''
    let pb'''' = Data.List.NonEmpty.map (\p -> runReader (setUID p) (SetUID "")) pb''' -- Data.List.NonEmpty.map unrollLoopsInPlay pb'''
    let pb''''' = Data.List.NonEmpty.map setReg pb''''
    let pb'''''' = Data.List.NonEmpty.map desugarBlocksInPlay pb'''''
    let pb''''''' = Data.List.NonEmpty.map unrollLoopsInPlay pb''''''
    let pb'''''''' = Data.List.NonEmpty.map desugarHandlersInPlay pb'''''''
    -- print pb'
    print pb''''''''
    return ()

main :: IO ()
main = do
    -- print "what"
    -- let s = traceShow "fucky boi" "l"
    -- print s
    -- testParseRoot
    -- testDirStacker
    -- testDSPlainRole
    -- testSVR
    -- testYaml
    -- testUnrollLoopForGenericMod
    -- testDesugarImports
    -- testUIDSetting
    -- testDrawArrows
    testWholeShebang "/home/sunrise/research/testansibleproj" "play"
    return ()
