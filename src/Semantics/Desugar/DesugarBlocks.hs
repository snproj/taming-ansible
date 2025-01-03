{-# LANGUAGE LambdaCase #-}
module Semantics.Desugar.DesugarBlocks where

import GrammarTypes.AnsibleGrammarTypes
import Data.List.NonEmpty (toList, fromList, NonEmpty, map)
import Control.Monad.Reader (MonadReader(ask), Reader, local, runReader)
import Text.Regex.TDFA.CorePattern (P(NonEmpty))
import qualified Text.Regex.TDFA.CorePattern as Data.List
import Data.Maybe (fromMaybe, fromJust)
import qualified Data.Map
import Data.Hashable (hash)

-- addRegister :: AttributeSet -> AttributeSet
-- addRegister attSet = let
--     existingReg = kwRegister attSet
--     newRed = case existingReg of
--         Nothing -> generateRegName attSet
--     in undefined
--         where
--             generateRegName :: AttributeSet -> String
--             generateRegName attSet = let
--                 res = case kwName attSet of
--                     Nothing -> hash

getSuccessIndicator :: Task -> JBE_EXP
getSuccessIndicator (Atomic attSet _ _) = let
    reg = fromJust $ atomicRegister attSet
    in JBE_EXP_BINARYOP
        (JBE_EXP_REGTEST (JBE_REG_R reg) JBE_TOP_IS JBE_TEST_DEFINED)
        JBE_OP_AND
        (JBE_EXP_REGTEST (JBE_REG_R reg) JBE_TOP_IS JBE_TEST_SUCCEEDED)
getSuccessIndicator (ContainingBlock _ block _) = let
    lastB = getSuccessIndicator $ last $ toList $ blockMain block
    lastR = (getSuccessIndicator . last . toList <$> rescue block)
    in case lastR of
        Just lastR' -> JBE_EXP_BINARYOP lastB JBE_OP_OR lastR'
        Nothing -> lastB

getFailedIndicator :: Task -> JBE_EXP
getFailedIndicator (Atomic attSet _ _) = let
    reg = fromJust $ atomicRegister attSet
    in JBE_EXP_BINARYOP
        (JBE_EXP_REGTEST (JBE_REG_R reg) JBE_TOP_IS JBE_TEST_DEFINED)
        JBE_OP_AND
        (JBE_EXP_REGTEST (JBE_REG_R reg) JBE_TOP_IS JBE_TEST_FAILED)
getFailedIndicator (ContainingBlock _ block _) = let
    in case rescue block of
        Just rescue' -> getFailedIndicator $ last $ toList rescue'
        Nothing -> orIndicatorsTogether $ Prelude.map getFailedIndicator $ toList $ blockMain block

-- updateAttributeSet :: (AttributeSet -> b) -> b -> TH a -> TH a
-- updateAttributeSet selector newValue (Atomic attSet modDecl) = let
--     selected = selector attSet
--     in Atomic attSet {selected = newValue} modDecl

updateWhen :: AtomicAttributeSet -> JBE_EXP -> AtomicAttributeSet
updateWhen attSet jjbeParen = let
    w = atomicWhen attSet
    existingJBEEXP = case w of
        VarContainingJinja (JBEPhrase existingJBEEXP') -> existingJBEEXP'
        SimpleVarBool b -> JBE_EXP_PRIM b
    w' = VarContainingJinja (JBEPhrase (
        JBE_EXP_BINARYOP (JBE_EXP_PARENEXP existingJBEEXP) JBE_OP_AND (JBE_EXP_PARENEXP jjbeParen)
        ))
    in attSet {atomicWhen=w'}

updateWhenBlock :: BlockAttributeSet -> JBE_EXP -> BlockAttributeSet
updateWhenBlock attSet jjbeParen =
  let w = blockWhen attSet
      existingJBEEXP = case w of
        VarContainingJinja (JBEPhrase existingJBEEXP') -> existingJBEEXP'
        SimpleVarBool b -> JBE_EXP_PRIM b
      w' =
        VarContainingJinja
          (JBEPhrase
              ( JBE_EXP_BINARYOP (JBE_EXP_PARENEXP existingJBEEXP) JBE_OP_AND (JBE_EXP_PARENEXP jjbeParen)
              )
          )
   in attSet {blockWhen = w'}

updateWhenTH :: Task -> JBE_EXP -> Task
updateWhenTH (Atomic attSet modDecl uid) jbeEXP = Atomic (updateWhen attSet jbeEXP) modDecl uid
updateWhenTH (ContainingBlock attSet block uid) jbeEXP = ContainingBlock (updateWhenBlock attSet jbeEXP) block uid

ignoreError :: Task -> Task
ignoreError (Atomic attSet modDecl uid) = Atomic attSet {atomicIgnoreErrors = SimpleVarBool True} modDecl uid
ignoreError (ContainingBlock _ _ _) = error "ERROR: For now, ContainingBlocks cannot have `ignoreError`."

atomizeBlockAttributes :: Task -> Reader (Maybe BlockAttributeSet) Task
atomizeBlockAttributes (Atomic aas modDecl uid) = do
    mbas <- ask
    case mbas of
        Nothing -> return $ Atomic aas modDecl uid
        Just bas -> let aas' = aas {
            atomicNotify=combineVarList (blockNotify bas) (atomicNotify aas),
            atomicWhen=combineWhen (blockWhen bas) (atomicWhen aas),
            atomicVars=combineVarList (blockVars bas) (atomicVars aas)
            }
            in return $ Atomic aas' modDecl uid
atomizeBlockAttributes (ContainingBlock bas blk uid) = do
    mParentBAS <- ask
    case mParentBAS of
        Nothing -> do
            bm <- traverse (local (const $ Just bas) . atomizeBlockAttributes) (blockMain blk)
            r <- traverse (traverse (local (const $ Just bas) . atomizeBlockAttributes)) (rescue blk)
            a <- traverse (traverse (local (const $ Just bas) . atomizeBlockAttributes)) (always blk)
            return $ ContainingBlock bas Block{blockMain=bm,rescue=r,always=a} uid
        Just parentBAS -> do
            let bas' = bas {
                blockNotify=combineVarList (blockNotify bas) (blockNotify parentBAS),
                blockWhen=combineWhen (blockWhen bas) (blockWhen parentBAS),
                blockVars = combineVarList (blockVars bas) (blockVars parentBAS)
                }
            bm <- traverse (local (const $ Just bas') . atomizeBlockAttributes) (blockMain blk)
            r <- traverse (traverse (local (const $ Just bas') . atomizeBlockAttributes)) (rescue blk)
            a <- traverse (traverse (local (const $ Just bas') . atomizeBlockAttributes)) (always blk)
            return $ ContainingBlock bas Block{blockMain=bm,rescue=r,always=a} uid

atomizeBlockAttributesInPlay :: Play -> Play
atomizeBlockAttributesInPlay p = let
    tl = Prelude.map (\th -> runReader (atomizeBlockAttributes th) Nothing) (tasks p)  
    hl = Prelude.map (\th -> runReader (atomizeBlockAttributes th) Nothing) (handlers p)
    in p {tasks=tl, handlers=hl}

getListOrSingleVal :: Var -> [Var]
getListOrSingleVal (ListVar ls) = ls
getListOrSingleVal (SimpleVarString s) = [SimpleVarString s]
getListOrSingleVal _ = error ""

combineVarList :: Maybe Var -> Maybe Var -> Maybe Var
combineVarList mv1 mv2 = case (mv1, mv2) of
    (Nothing, Nothing) -> Nothing
    (Just v1, Nothing) -> Just v1
    (Nothing, Just v2) -> Just v2
    (Just v1, Just v2) -> let
        v1' = getListOrSingleVal v1
        v2' = getListOrSingleVal v2
        in Just $ ListVar $ v1' ++ v2'

combineWhen :: Var -> Var -> Var
combineWhen v1 v2 = let
    VarContainingJinja (JBEPhrase jbe1) = v1
    VarContainingJinja (JBEPhrase jbe2) = v2
    newJBE = JBE_EXP_BINARYOP jbe1 JBE_OP_OR jbe2
    in VarContainingJinja $ JBEPhrase $ newJBE



-- propagateAttributeSetWithinBlock :: AttributeSet -> [TH a] -> [TH a]
-- propagateAttributeSetWithinBlock attSet = Prelude.map (applyAttributeSetToTH attSet)
--     where
--         mergeAttributeSets :: AttributeSet -> AttributeSet -> AttributeSet
--         mergeAttributeSets = undefined
--         applyAttributeSetToTH :: AttributeSet -> TH a -> TH a
--         applyAttributeSetToTH parentAttSet (Atomic localAttSet modDecl) = Atomic (mergeAttributeSets parentAttSet localAttSet) modDecl
--         applyAttributeSetToTH parentAttSet (ContainingBlock localAttSet block) = let
--             merged = mergeAttributeSets parentAttSet localAttSet


-- invalid attributes for blocks: changed_when, failed_when, loop, retries, force_handlers, register
-- attributes that get overwritten by more local definitions: notify, var, when
-- mergeAttributeSets :: AttributeSet -> AttributeSet -> AttributeSet -- assumed that block is parent and atomic is child; this is overly permissive for block to block
-- mergeAttributeSets parentAttSet childAttSet = AttributeSet {
--     kwName=arrange kwName mergeNames,
--     kwForceHandlers=arrange kwForceHandlers neither,
--     kwNotify=arrange kwNotify onlyChooseRight,
--     kwLoop=arrange kwLoop onlyChooseRight,
--     kwWhen=arrange kwWhen chooseRight,
--     kwVars=arrange kwVars mergeVars,
--     kwChangedWhen=arrange kwChangedWhen onlyChooseRight,
--     kwFailedWhen=arrange kwFailedWhen onlyChooseRight,
--     kwUntil=arrange kwUntil onlyChooseRight,
--     kwRetries=arrange kwRetries onlyChooseRight,
--     kwRegister=arrange kwRegister onlyChooseRight,
--     kwIgnoreErrors = arrange kwIgnoreErrors chooseRight
-- }
--     where
--         extractBoth :: (AttributeSet -> a) -> AttributeSet -> AttributeSet -> (a -> a -> b) -> b
--         extractBoth selector attSet1 attSet2 func = func (selector attSet1) (selector attSet2)
--         onlyChooseRight :: Maybe a -> Maybe a -> Maybe a
--         onlyChooseRight x y = case x of
--             Just _ -> error "ERROR: Invalid attribute for block!"
--             Nothing -> y
--         chooseRight = flip const
--         neither :: Maybe a -> Maybe a -> Maybe a
--         neither Nothing Nothing = Nothing
--         neither _ _ = error "ERROR: Invalid attribute for block and/or task!"
--         mergeVars :: Maybe Var -> Maybe Var -> Maybe Var
--         mergeVars var1 var2 = let
--             getVar = \v -> fromMaybe (DictVar Data.Map.empty) v
--             getMSV = \(DictVar msv) -> msv -- should panic if not DictVar
--             unioned = Data.Map.union ((getMSV . getVar) var1) ((getMSV . getVar) var2)
--             in if null unioned
--                 then Nothing
--                 else Just $ DictVar unioned
--         mergeNames :: Maybe String -> Maybe String -> Maybe String
--         mergeNames ms1 ms2 = Just $ fromMaybe "NONAME_LEFT" ms1 ++ fromMaybe "NONAME_RIGHT" ms2
--         arrange s = extractBoth s parentAttSet childAttSet

-- desugarAttributeSetsWithinBlock :: TH a -> TH a
-- desugarAttributeSetsWithinBlock (Atomic _ _ _) = error "ERROR: desugarAttributeSetsWithinBlock was called on Atomic!"
-- desugarAttributeSetsWithinBlock (ContainingBlock attSet block uid) = squishBlockAttributeSetsIntoAtomics AttributeSet {} (ContainingBlock attSet block uid)
--     where
--         squishBlockAttributeSetsIntoAtomics :: AttributeSet -> TH a -> TH a
--         squishBlockAttributeSetsIntoAtomics parentAttSet (Atomic localAttSet modDecl uid') = Atomic (mergeAttributeSets parentAttSet localAttSet) modDecl uid'
--         squishBlockAttributeSetsIntoAtomics parentAttSet (ContainingBlock localAttSet (Block _blockMain _rescue _always) uid') = let
--             merged = mergeAttributeSets parentAttSet localAttSet
--             mapApplyAtt = Prelude.map (squishBlockAttributeSetsIntoAtomics merged) . toList
--             _blockMain' = mapApplyAtt _blockMain
--             _rescue' = fmap mapApplyAtt _rescue
--             _always' = fmap mapApplyAtt _always
--             in ContainingBlock AttributeSet {} Block {
--                 blockMain = fromList _blockMain',
--                 rescue = fmap fromList _rescue',
--                 always = fmap fromList _always'
--             } uid'


-- Always draw on the second one!
linkToPrevTH :: Task -> Task -> Task
-- linkToPrevTH (Atomic _ _ _) (Atomic x y uid) = Atomic x y uid
linkToPrevTH prev (Atomic attSet2 modDecl uid) = let
    succIndicator = getSuccessIndicator prev
    newAttSet = updateWhen attSet2 succIndicator
    in Atomic newAttSet modDecl uid
linkToPrevTH prev (ContainingBlock attSet2 block2 uid) = let
    -- settle links within block first
    ContainingBlock attSet2' block2' uid' = drawArrowsWithinBlock (ContainingBlock attSet2 block2 uid)

    -- then connect to prev task
    _blockMain = blockMain block2'
    blockMainList = toList _blockMain
    b = head blockMainList
    b' = linkToPrevTH prev b
    newBlockMain = fromList (b' : tail blockMainList)
    newAlways = do
        _always <- always block2'
        let alwaysList = toList _always
        let a = head alwaysList
        let a' = linkToPrevTH prev a
        return (fromList (a' : tail alwaysList))
    in ContainingBlock attSet2' (Block {
        blockMain=newBlockMain,
        rescue=rescue block2',
        always=newAlways
    }) uid'

drawArrowsWithinBlock :: Task -> Task
drawArrowsWithinBlock (Atomic _ _ _) = error "ERROR: Cannot call drawArrowsWithinBlock on Atomic!"
drawArrowsWithinBlock (ContainingBlock attSet block uid) = let
    _blockMain = iE . drawArrowsWithinBlockMain $ blockMain block
    _rescue = iEM  (drawArrowsWithinRescue (rescue block) _blockMain)
    _always = iEM . drawArrowsWithinAlways $ always block
    in ContainingBlock attSet Block {
        blockMain=_blockMain,
        rescue=_rescue,
        always=_always
    } uid
        where
            iE :: NonEmpty Task -> NonEmpty Task
            iE = Data.List.NonEmpty.map ignoreError
            iEM :: Maybe (NonEmpty Task) -> Maybe (NonEmpty Task)
            iEM = fmap iE


drawArrowsWithinBlockMain :: NonEmpty Task -> NonEmpty Task
drawArrowsWithinBlockMain _blockMain = let
    blockMainList = toList _blockMain
    chained = chainTogether blockMainList
    in fromList chained

chainTogether :: [Task] -> [Task]
chainTogether thList = head thList : zipWith linkToPrevTH thList (tail thList)

desugarBlocksInPlay :: Play -> Play
desugarBlocksInPlay p = let
    p' = atomizeBlockAttributesInPlay p
    tl = (flattenBlocks . chainTogether) (tasks p')
    hl = (flattenBlocks . chainTogether) (handlers p')
    in p' {tasks=tl,handlers=hl}

drawArrowsWithinRescue :: Maybe (NonEmpty Task) -> NonEmpty Task -> Maybe (NonEmpty Task)
drawArrowsWithinRescue _rescue _blockMain = do
    _rescue' <- _rescue
    let rescueList = toList _rescue'
    let blockMainList = toList _blockMain
    let allBlockMainFailIndicators = Prelude.map getFailedIndicator blockMainList
    let orred = orIndicatorsTogether allBlockMainFailIndicators
    let firstRescue = head rescueList
    let firstRescue' = updateWhenTH firstRescue orred
    let rescueList' = firstRescue' : tail rescueList
    let chained = chainTogether rescueList'
    return $ fromList chained

drawArrowsWithinAlways :: Maybe (NonEmpty Task) -> Maybe (NonEmpty Task)
drawArrowsWithinAlways _always = do
    _always' <- _always
    let alwaysList = toList _always'
    let chained = chainTogether alwaysList
    return $ fromList chained

orIndicatorsTogether :: [JBE_EXP] -> JBE_EXP
orIndicatorsTogether = foldl (`JBE_EXP_BINARYOP` JBE_OP_OR) (JBE_EXP_PRIM False)

flattenBlocks :: [Task] -> [Task]
flattenBlocks = concatMap flattenBlock
    where
        flattenBlock :: Task -> [Task]
        flattenBlock (Atomic x y z) = [Atomic x y z]
        flattenBlock (ContainingBlock _ (Block _blockMain _rescue _always) _) = let
            bml = toList _blockMain
            rl = maybe [] toList _rescue
            al = maybe [] toList _always
            in bml ++ rl ++ al
