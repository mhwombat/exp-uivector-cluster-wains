------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.UIVector.Cluster.Experiment
-- Copyright   :  (c) Amy de Buitléir 2012-2016
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- A data mining agent, designed for the Créatúr framework.
--
------------------------------------------------------------------------
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types #-}
module ALife.Creatur.Wain.UIVector.Cluster.Experiment
  (
    PatternWain,
    PatternTweaker(..),
    run,
    randomPatternWain,
    finishRound,
    schemaQuality,
    printStats,
    versionInfo,
    idealPopControlDeltaE -- exported for testing only
  ) where

import ALife.Creatur (agentId, isAlive, programVersion)
import ALife.Creatur.Task (checkPopSize)
import ALife.Creatur.Persistent (putPS, getPS)
import qualified ALife.Creatur.Wain as W
import ALife.Creatur.Wain.Brain (Brain, classifier, predictor,
  decisionQuality, makeBrain, scenarioReport, responseReport,
  decisionReport)
import ALife.Creatur.Wain.Checkpoint (enforceAll)
import qualified ALife.Creatur.Wain.Classifier as Cl
import ALife.Creatur.Wain.Muser (makeMuser)
import ALife.Creatur.Wain.Predictor(buildPredictor)
import ALife.Creatur.Wain.GeneticSOM (RandomLearningParams(..),
  GeneticSOM, randomLearningFunction, schemaQuality)
import qualified ALife.Creatur.Wain.UIVector.Object as O
import ALife.Creatur.Wain.UIVector.Pattern (Pattern)
import ALife.Creatur.Wain.UIVector.PatternDB (PatternDB, anyPattern)
import ALife.Creatur.Wain.UIVector.Tweaker (PatternTweaker(..))
import ALife.Creatur.Wain.Pretty (pretty)
import ALife.Creatur.Wain.Raw (raw)
import ALife.Creatur.Wain.Response (Response, _action, _outcomes)
import ALife.Creatur.Wain.UnitInterval (UIDouble, uiToDouble)
import qualified ALife.Creatur.Wain.Statistics as Stats
import ALife.Creatur.Wain.UIVector.Cluster.Action (Action(..), numActions)
import qualified ALife.Creatur.Wain.UIVector.Wain as PW
import qualified ALife.Creatur.Wain.UIVector.Cluster.Universe as U
import qualified ALife.Creatur.Wain.UIVector.Wain as UW
import ALife.Creatur.Wain.PersistentStatistics (updateStats, readStats,
  clearStats)
import ALife.Creatur.Wain.Statistics (summarise)
import ALife.Creatur.Util (stateMap)
import ALife.Creatur.Wain.Weights (makeWeights)
import Control.Conditional (whenM)
import Control.Lens hiding (universe)
import Control.Monad (when, unless)
import Control.Monad.Catch (catchAll)
import Control.Exception (SomeException)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Random (Rand, RandomGen, getRandomR, getRandomRs,
  getRandom, evalRandIO, fromList)
import Control.Monad.State.Lazy (StateT, execStateT, evalStateT, get)
import qualified Data.ByteString as BS
import Data.List (intercalate, minimumBy)
import Data.Ord (comparing)
import Data.Version (showVersion)
import qualified Data.Serialize as DS 
import Data.Word (Word64)
import Paths_exp_uivector_cluster_wains (version)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (dropFileName)
import Text.Printf (printf)

versionInfo :: String
versionInfo
  = "exp-uivector-cluster-wains-" ++ showVersion version
      ++ ", compiled with " ++ UW.packageVersion
      ++ ", " ++ W.packageVersion
      ++ ", " ++ ALife.Creatur.programVersion

type PatternWain = PW.PatternWain Action
type Object = O.Object Action

randomPatternWain
  :: RandomGen r
    => String -> U.Universe PatternWain -> Word64 -> Rand r PatternWain
randomPatternWain wName u classifierSize = do
  let fcp = RandomLearningParams
               { _r0Range = view U.uClassifierR0Range u,
                 _rfRange = view U.uClassifierRfRange u,
                 _tfRange = view U.uClassifierTfRange u }
  fc <- randomLearningFunction fcp
  classifierThreshold <- getRandomR (view U.uClassifierThresholdRange u)
  tw <- makeWeights . take (view U.uVectorLength u) <$> getRandomRs (0,1)
  let c = Cl.buildClassifier fc classifierSize classifierThreshold
            (PatternTweaker tw)
  let fdp = RandomLearningParams
              { _r0Range = view U.uPredictorR0Range u,
                _rfRange = view U.uPredictorRfRange u,
                _tfRange = view U.uPredictorTfRange u }
  fd <- randomLearningFunction fdp
  predictorThreshold <- getRandomR (view U.uPredictorThresholdRange u)
  let predictorSize = classifierSize * fromIntegral numActions
  let dr = buildPredictor fd predictorSize predictorThreshold
  -- TODO: Allow a range of random weights
  -- hw <- (makeWeights . take 3) <$> getRandomRs unitInterval
  let hw = makeWeights [0.7, 0.3, 0, 0.1]
  dOut <- take 4 <$> getRandomRs (view U.uDefaultOutcomeRange u)
  dp <- getRandomR $ view U.uDepthRange u
  let mr = makeMuser dOut dp
  t <- getRandom
  s <- getRandomR (view U.uStrictnessRange u)
  ios <- take 4 <$> getRandomRs (view U.uImprintOutcomeRange u)
  rds <- take 4 <$> getRandomRs (view U.uReinforcementDeltasRange u)
  let (Right wBrain) = makeBrain c mr dr hw t s ios rds
  wDevotion <- getRandomR . view U.uDevotionRange $ u
  wAgeOfMaturity <- getRandomR . view U.uMaturityRange $ u
  wPassionDelta <- getRandomR . view U.uBoredomDeltaRange $ u
  wBoredomDelta <- getRandomR . view U.uPassionDeltaRange $ u
  let wAppearance = replicate (view U.uVectorLength u) 0
  return $ W.buildWainAndGenerateGenome wName wAppearance wBrain
    wDevotion wAgeOfMaturity wPassionDelta wBoredomDelta

data Summary = Summary
  {
    _rPopSize :: Int,
    _rDirectObjectNovelty :: UIDouble,
    _rDirectObjectAdjustedNovelty :: Int,
    _rIndirectObjectNovelty :: UIDouble,
    _rIndirectObjectAdjustedNovelty :: Int,
    _rOtherNovelty :: UIDouble,
    _rOtherAdjustedNovelty :: Int,
    _rMetabolismDeltaE :: Double,
    _rCSQDeltaE :: Double,
    _rDSQDeltaE :: Double,
    _rDQDeltaE :: Double,
    _rPopControlDeltaE :: Double,
    _rCoopDeltaE :: Double,
    _rAgreementDeltaE :: Double,
    _rFlirtingDeltaE :: Double,
    _rMatingDeltaE :: Double,
    _rOldAgeDeltaE :: Double,
    _rOtherMatingDeltaE :: Double,
    _rOtherAgreementDeltaE :: Double,
    _rNetDeltaE :: Double,
    _rChildNetDeltaE :: Double,
    _rDeltaEToReflectOn :: Double,
    _rDeltaBToReflectOn :: Double,
    _rDeltaPToReflectOn :: Double,
    _rDeltaHToReflectOn :: Double,
    _rErr :: Double,
    _rBirthCount :: Int,
    _rWeanCount :: Int,
    _rCooperateCount :: Int,
    _rAgreeCount :: Int,
    _rFlirtCount :: Int,
    _rMateCount :: Int,
    _rIgnoreCount :: Int,
    _rDeathCount :: Int,
    _rMistakeCount :: Int
  }
makeLenses ''Summary

initSummary :: Int -> Summary
initSummary p = Summary
  {
    _rPopSize = p,
    _rDirectObjectNovelty = 0,
    _rDirectObjectAdjustedNovelty = 0,
    _rIndirectObjectNovelty = 0,
    _rIndirectObjectAdjustedNovelty = 0,
    _rOtherNovelty = 0,
    _rOtherAdjustedNovelty = 0,
    _rMetabolismDeltaE = 0,
    _rCSQDeltaE = 0,
    _rDSQDeltaE = 0,
    _rDQDeltaE = 0,
    _rPopControlDeltaE = 0,
    _rCoopDeltaE = 0,
    _rAgreementDeltaE = 0,
    _rFlirtingDeltaE = 0,
    _rMatingDeltaE = 0,
    _rOldAgeDeltaE = 0,
    _rOtherMatingDeltaE = 0,
    _rOtherAgreementDeltaE = 0,
    _rNetDeltaE = 0,
    _rChildNetDeltaE = 0,
    _rDeltaEToReflectOn = 0,
    _rDeltaBToReflectOn = 0,
    _rDeltaPToReflectOn = 0,
    _rDeltaHToReflectOn = 0,
    _rErr = 0,
    _rBirthCount = 0,
    _rWeanCount = 0,
    _rCooperateCount = 0,
    _rAgreeCount = 0,
    _rFlirtCount = 0,
    _rMateCount = 0,
    _rIgnoreCount = 0,
    _rDeathCount = 0,
    _rMistakeCount = 0
  }

summaryStats :: Summary -> [Stats.Statistic]
summaryStats r =
  [
    Stats.dStat "pop. size" (view rPopSize r),
    Stats.dStat "DO novelty" (view rDirectObjectNovelty r),
    Stats.iStat "DO novelty (adj.)"
      (view rDirectObjectAdjustedNovelty r),
    Stats.dStat "IO novelty" (view rIndirectObjectNovelty r),
    Stats.iStat "IO novelty (adj.)"
      (view rIndirectObjectAdjustedNovelty r),
    Stats.dStat "novelty to other" (view rOtherNovelty r),
    Stats.iStat "novelty to other (adj.)"
      (view rOtherAdjustedNovelty r),
    Stats.dStat "metabolism Δe" (view rMetabolismDeltaE r),
    Stats.dStat "CSQ Δe" (view rCSQDeltaE r),
    Stats.dStat "DSQ Δe" (view rDSQDeltaE r),
    Stats.dStat "DQ Δe" (view rDQDeltaE r),
    Stats.dStat "pop. control Δe" (view rPopControlDeltaE r),
    Stats.dStat "cooperation Δe" (view rCoopDeltaE r),
    Stats.dStat "agreement Δe" (view rAgreementDeltaE r),
    Stats.dStat "flirting Δe" (view rFlirtingDeltaE r),
    Stats.dStat "mating Δe" (view rMatingDeltaE r),
    Stats.dStat "old age Δe" (view rOldAgeDeltaE r),
    Stats.dStat "other mating Δe" (view rOtherMatingDeltaE r),
    Stats.dStat "other agreement Δe" (view rOtherAgreementDeltaE r),
    Stats.dStat "net Δe" (view rNetDeltaE r),
    Stats.dStat "child net Δe" (view rChildNetDeltaE r),
    Stats.dStat "Δe to reflect on" (view rDeltaEToReflectOn r),
    Stats.dStat "Δb to reflect on" (view rDeltaBToReflectOn r),
    Stats.dStat "Δp to reflect on" (view rDeltaPToReflectOn r),
    Stats.dStat "Δh to reflect on" (view rDeltaHToReflectOn r),
    Stats.dStat "err" (view rErr r),
    Stats.iStat "bore" (view rBirthCount r),
    Stats.iStat "weaned" (view rWeanCount r),
    Stats.iStat "co-operated" (view rCooperateCount r),
    Stats.iStat "agreed" (view rAgreeCount r),
    Stats.iStat "flirted" (view rFlirtCount r),
    Stats.iStat "mated" (view rMateCount r),
    Stats.iStat "ignored" (view rIgnoreCount r),
    Stats.iStat "died" (view rDeathCount r),
    Stats.iStat "mistakes" (view rMistakeCount r)
  ]

data Experiment = Experiment
  {
    _subject :: PatternWain,
    _directObject :: Object,
    _indirectObject :: Object,
    _weanlings :: [PatternWain],
    _universe :: U.Universe PatternWain,
    _summary :: Summary
  }
makeLenses ''Experiment

-- TODO: Is there a more lens-y way to do this? Maybe can combine a
-- prism and a lens.
directObjectWain :: Lens' Experiment PatternWain
directObjectWain = lens getWain setWain
  where
    getWain :: Experiment -> PatternWain
    getWain = O.objectToWain . _directObject

    setWain :: Experiment -> PatternWain -> Experiment
    setWain e w = e { _directObject = O.AObject w }

indirectObjectWain :: Lens' Experiment PatternWain
indirectObjectWain = lens getWain setWain
  where
    getWain :: Experiment -> PatternWain
    getWain = O.objectToWain . _indirectObject

    setWain :: Experiment -> PatternWain -> Experiment
    setWain e w = e { _indirectObject = O.AObject w }

report :: String -> StateT Experiment IO ()
report = zoom universe . U.writeToLog

run :: [PatternWain] -> StateT (U.Universe PatternWain) IO [PatternWain]
run (me:w1:w2:xs) = do
  when (null xs) $ U.writeToLog "WARNING: Last wain standing!"
  u <- get
  (x, y) <- liftIO $ chooseObjects (view U.uFrequencies u) w1 w2
                       (view U.uPatternDB u)
  p <- U.popSize
  let e = Experiment { _subject = me,
                       _directObject = x,
                       _indirectObject = y,
                       _weanlings = [],
                       _universe = u,
                       _summary = initSummary p}
  e' <- liftIO $ execStateT runWrapped e
  let modifiedAgents = O.addIfWain (view directObject e')
        . O.addIfWain (view indirectObject e')
            $ view subject e' : view weanlings e'
  U.writeToLog $
    "Modified agents: " ++ show (map agentId modifiedAgents)
  reportAnyDeaths modifiedAgents
  return modifiedAgents
run _ = error "too few wains"

runWrapped :: StateT Experiment IO ()
runWrapped = catchAll run' reportException

withUniverse
  :: Monad m
    => StateT (U.Universe PatternWain) m a -> StateT Experiment m a
withUniverse program = do
  e <- get
  stateMap (\u -> e { _universe=u }) _universe program

reportException :: SomeException -> StateT Experiment IO ()
reportException e = do
  report $ "WARNING: Unhandled exception: " ++ show e
  t <- withUniverse U.currentTime
  a <- use subject
  d <- use (universe . U.uDebugDir)
  let f = d ++ "/" ++ show t ++ "_" ++ agentId a
  report $ "Saving debug info as " ++ f
  liftIO $ BS.writeFile (f ++ ".subject") (DS.encode a)
  b <- use directObject
  liftIO $ BS.writeFile (f ++ ".dObj") (DS.encode b)
  c <- use indirectObject
  liftIO $ BS.writeFile (f ++ ".iObj") (DS.encode c)

run' :: StateT Experiment IO ()
run' = do
  (e0, ec0) <- totalEnergy
  a <- use subject
  report $ "---------- " ++ agentId a ++ "'s turn ----------"
  report $ "At beginning of turn, " ++ agentId a
    ++ "'s summary: " ++ pretty (Stats.stats a)
  runMetabolism
  applySQEffects classifier U.uCSQDeltaE rCSQDeltaE
  applySQEffects predictor U.uDSQDeltaE rDSQDeltaE
  applyDQEffects
  autoPopControl <- use (universe . U.uPopControl)
  when autoPopControl applyPopControl
  r <- chooseSubjectAction
  wainBeforeAction <- use subject
  runAction (_action r)
  letSubjectReflect wainBeforeAction r
  subject %= W.autoAdjustPassion
  -- subject %= W.autoAdjustBoredom
  subject %= W.incAge
  a' <- use subject
  -- assign (summary.rNetDeltaE) (energy a' - energy a)
  unless (isAlive a') $ assign (summary.rDeathCount) 1
  summary %= fillInSummary
  (ef, ecf) <- totalEnergy
  balanceEnergyEquation e0 ec0 ef ecf
  updateChildren
  killIfTooOld
  agentStats <- ((Stats.stats a' ++) . summaryStats) <$> use summary
  report $ "At end of turn, " ++ agentId a
    ++ "'s summary: " ++ pretty agentStats
  rsf <- use (universe . U.uRawStatsFile)
  zoom universe $ writeRawStats (agentId a) rsf agentStats
  sf <- use (universe . U.uStatsFile)
  zoom universe $ updateStats agentStats sf

fillInSummary :: Summary -> Summary
fillInSummary s = s
  {
    _rNetDeltaE = _rMetabolismDeltaE s
         + _rCSQDeltaE s
         + _rDSQDeltaE s
         + _rDQDeltaE s
         + _rPopControlDeltaE s
         + _rCoopDeltaE s
         + _rAgreementDeltaE s
         + _rFlirtingDeltaE s
         + _rMatingDeltaE s
         + _rOldAgeDeltaE s
         + _rOtherMatingDeltaE s
         + _rOtherAgreementDeltaE s,
    _rChildNetDeltaE = 0
         -- include energy given to wains when they are born
         - _rMatingDeltaE s
         - _rOtherMatingDeltaE s
  }

balanceEnergyEquation
  :: Double -> Double -> Double -> Double -> StateT Experiment IO ()
balanceEnergyEquation e0 ec0 ef ecf = do
  netDeltaE1 <- use (summary . rNetDeltaE)
  let netDeltaE2 = ef - e0
  let err = abs (netDeltaE1 - netDeltaE2)
  when (err > 0.000001) $ do
    report $ "WARNING: Adult energy equation doesn't balance"
    report $ "e0=" ++ show e0 ++ ", ef=" ++ show ef
      ++ ", netDeltaE2=" ++ show netDeltaE2
      ++ ", netDeltaE1=" ++ show netDeltaE1
      ++ ", err=" ++ show err
  childNetDeltaE1 <- use (summary . rChildNetDeltaE)
  let childNetDeltaE2 = ecf - ec0
  let childErr = abs (childNetDeltaE1 - childNetDeltaE2)
  when (childErr > 0.000001) $ do
    report $ "WARNING: Child energy equation doesn't balance"
    report $ "ec0=" ++ show ec0 ++ ", ecf=" ++ show ecf
      ++ ", childNetDeltaE2=" ++ show childNetDeltaE2
      ++ ", childNetDeltaE1=" ++ show childNetDeltaE1
      ++ ", childErr=" ++ show childErr

runMetabolism :: StateT Experiment IO ()
runMetabolism = do
  a <- use subject
  bmc <- use (universe . U.uBaseMetabolismDeltaE)
  cpcm <- use (universe . U.uEnergyCostPerClassifierModel)
  ccf <- use (universe . U.uChildCostFactor)
  let deltaE = PW.metabCost bmc cpcm 1 a
                 + sum (map (PW.metabCost bmc cpcm ccf)
                         (view W.litter a))
  PW.adjustEnergy subject deltaE rMetabolismDeltaE "metab." summary
    report

chooseSubjectAction
  :: StateT Experiment IO (Response Action)
chooseSubjectAction = do
  a <- use subject
  dObj <- use directObject
  iObj <- use indirectObject
  (dObjNovelty, dObjNoveltyAdj, iObjNovelty, iObjNoveltyAdj, r, a')
    <- zoom universe $ chooseAction3 a dObj iObj
  assign (summary.rDirectObjectNovelty) dObjNovelty
  assign (summary.rDirectObjectAdjustedNovelty) dObjNoveltyAdj
  assign (summary.rIndirectObjectNovelty) iObjNovelty
  assign (summary.rIndirectObjectAdjustedNovelty) iObjNoveltyAdj
  assign subject a'
  return r

choosePartnerAction
  :: StateT Experiment IO (Response Action)
choosePartnerAction = do
  a <- use subject
  dObj <- use directObject
  b <- use indirectObjectWain
  (dObjNovelty, dObjNoveltyAdj, _, _, r, b')
    <- zoom universe $ chooseAction3 b dObj (O.AObject a)
  assign (summary.rOtherNovelty) dObjNovelty
  assign (summary.rOtherAdjustedNovelty) dObjNoveltyAdj
  assign indirectObjectWain b'
  return r

chooseAction3
  :: PatternWain -> Object -> Object
    -> StateT (U.Universe PatternWain) IO
        (UIDouble, Int, UIDouble, Int, Response Action, PatternWain)
chooseAction3 w dObj iObj = do
  U.writeToLog $ agentId w ++ " sees " ++ O.objectId dObj
    ++ " and " ++ O.objectId iObj
  whenM (use U.uShowPredictorModels)
    (mapM_ U.writeToLog . PW.describePredictorModels $ w)
  let (lds, sps, rplos, aos, r, w')
        = W.chooseAction
            [O.objectAppearance dObj, O.objectAppearance iObj] w
  let (_, dObjNovelty, dObjNoveltyAdj,
        _, iObjNovelty, iObjNoveltyAdj)
          = analyseClassification lds w
  whenM (use U.uGenFmris)
    (mapM_ U.writeToLog . PW.describeClassifierModels $ w)
  U.writeToLog $ "To " ++ agentId w ++ ", "
    ++ O.objectId dObj ++ " has novelty " ++ show dObjNovelty
  U.writeToLog $ "To " ++ agentId w ++ ", "
    ++ O.objectId iObj ++ " has novelty " ++ show iObjNovelty
  whenM (use U.uShowPredictions) $ do
    mapM_ U.writeToLog $ scenarioReport sps
    mapM_ U.writeToLog $ responseReport rplos
    mapM_ U.writeToLog $ decisionReport aos
  U.writeToLog $ "To " ++ agentId w ++ ", "
    ++ O.objectId dObj ++ " has adjusted novelty " ++ show dObjNoveltyAdj
  U.writeToLog $ "To " ++ agentId w ++ ", "
    ++ O.objectId iObj ++ " has adjusted novelty " ++ show iObjNoveltyAdj
  U.writeToLog $ agentId w ++ " sees " ++ O.objectId dObj
    ++ " and chooses to " ++ show (_action r)
    ++ " predicting the outcomes " ++ show (_outcomes r)
  return (dObjNovelty, dObjNoveltyAdj, iObjNovelty, iObjNoveltyAdj, r, w')

analyseClassification
  :: [[(Cl.Label, Cl.Difference)]] -> PatternWain
    -> (Cl.Label, Cl.Difference, Int, Cl.Label, Cl.Difference, Int)
analyseClassification ldss w
  = (dObjLabel, dObjNovelty, dObjNoveltyAdj,
      iObjLabel, iObjNovelty, iObjNoveltyAdj)
  where ((dObjLabel, dObjNovelty):(iObjLabel, iObjNovelty):_)
          = map (minimumBy (comparing snd)) ldss
        dObjNoveltyAdj
          = round $ uiToDouble dObjNovelty * fromIntegral (view W.age w)
        iObjNoveltyAdj
          = round $ uiToDouble iObjNovelty * fromIntegral (view W.age w)

chooseObjects
  :: [Rational] -> PatternWain -> PatternWain -> PatternDB
    -> IO (Object, Object)
chooseObjects freqs w1 w2 db = do
  (vId1, v1) <- evalStateT anyPattern db
  (vId2, v2) <- evalStateT anyPattern db
  choosePair freqs (O.PObject v1 vId1, O.PObject v2 vId2)
    (O.AObject w1, O.AObject w2)

choosePair :: [Rational] -> (a, a) -> (a, a) -> IO (a, a)
choosePair freqs (i1, i2) (w1, w2)
  = fromList $ zip [(i1, i2), (i1, w1), (w1, i1), (w1, w2)] freqs

runAction :: Action -> StateT Experiment IO ()

--
-- Flirt
--
runAction Flirt = do
  applyFlirtationEffects
  a <- use subject
  dObj <- use directObject
  report $ agentId a ++ " flirts with " ++ O.objectId dObj
  unless (O.isPattern dObj) flirt

--
-- Ignore
--
runAction Ignore = do
  a <- use subject
  dObj <- use directObject
  report $ agentId a ++ " ignores " ++ O.objectId dObj
  (summary.rIgnoreCount) += 1

--
-- Co-operate
--
runAction aAction = do
  applyCooperationEffects
  a <- use subject
  dObj <- use directObject
  iObj <- use indirectObject
  case iObj of
    O.AObject b   -> do
      report $ agentId a ++ " tells " ++ agentId b
        ++ " that record " ++ O.objectId dObj ++ " has label "
        ++ show aAction
      r <- choosePartnerAction
      let bAction = _action r
      if aAction == bAction
        then do
          report $ agentId b ++ " agrees with " ++  agentId a
            ++ " that " ++ O.objectId dObj
            ++ " has label " ++ show aAction
          applyAgreementEffects
        else do
          report $ agentId b ++ " disagrees with " ++ agentId a
            ++ ", says that " ++ O.objectId dObj
            ++ " has label " ++ show bAction
          applyDisagreementEffects aAction bAction
    O.PObject _ _ -> do
      report $ "Attempting to co-operate with a data record"
      return ()

--
-- Utility functions
--

applySQEffects
  :: Simple Lens (Brain Pattern PatternTweaker  Action) (GeneticSOM p t)
    -> Simple Lens (U.Universe PatternWain) Double
     -> Simple Lens Summary Double -> StateT Experiment IO ()
applySQEffects component deltaESelector adultSelector = do
  aSQ <- fromIntegral . schemaQuality
          <$> use (subject . W.brain . component)
  x <- use (universe . deltaESelector)
  let deltaE = x*aSQ
  PW.adjustEnergy subject deltaE adultSelector "SQ" summary
    report

applyDQEffects :: StateT Experiment IO ()
applyDQEffects = do
  aDQ <- fromIntegral . decisionQuality <$> use (subject . W.brain)
  x <- use (universe . U.uDQDeltaE)
  let deltaE = x*aDQ
  PW.adjustEnergy subject deltaE rDQDeltaE "DQ" summary report

applyPopControl :: StateT Experiment IO ()
applyPopControl = do
  deltaE <- zoom (universe . U.uPopControlDeltaE) getPS
  PW.adjustEnergy subject deltaE rPopControlDeltaE
    "pop. control" summary report

applyCooperationEffects :: StateT Experiment IO ()
applyCooperationEffects = do
  deltaE <- use (universe . U.uCooperationDeltaE)
  report $ "Applying co-operation energy adjustment"
  PW.adjustEnergy subject deltaE rCoopDeltaE "cooperation" summary
    report
  (summary.rCooperateCount) += 1

applyAgreementEffects :: StateT Experiment IO ()
applyAgreementEffects = do
  a <- use subject
  b <- use indirectObjectWain
  dObj <- use directObject
  if O.isPattern dObj
    then do
      let aDQ = fromIntegral . decisionQuality $ view W.brain a
      let bDQ = fromIntegral . decisionQuality $ view W.brain b
      aNovelty <- uiToDouble <$> use (summary . rDirectObjectNovelty)
      bNovelty <- uiToDouble <$> use (summary . rOtherNovelty)
      xd <- use (universe . U.uDQBasedAgreementDeltaE)
      xn <- use (universe . U.uNoveltyBasedAgreementDeltaE)
      x0 <- use (universe . U.uMinAgreementDeltaE)
      let ra = x0 + xn * aNovelty + xd * aDQ
      report $ "Applying agreement energy adjustment"
      PW.adjustEnergy subject ra rAgreementDeltaE "agreement"
        summary report
      let rb = x0 + xn * bNovelty + xd * bDQ
      report $ "Applying agreement energy adjustment"
      PW.adjustEnergy indirectObjectWain rb rOtherAgreementDeltaE
        "agreement" summary report
      (summary.rAgreeCount) += 1
    else
      report "No reward for agreeing on a classification for a wain"

applyDisagreementEffects :: Action -> Action -> StateT Experiment IO ()
applyDisagreementEffects aAction bAction = do
  aNovelty <- uiToDouble <$> use (summary . rDirectObjectNovelty)
  bNovelty <- uiToDouble <$> use (summary . rOtherNovelty)
  a <- use subject
  b <- use indirectObjectWain
  pa <- view W.appearance <$> use subject
  dObj <- use directObject
  let p1 = O.objectAppearance dObj
  let pb = view W.appearance b
  -- report $ "DEBUG aNovelty=" ++ show aNovelty
  -- report $ "DEBUG bNovelty=" ++ show bNovelty
  let aConfidence = (1 - aNovelty)*(fromIntegral . view W.age $ a)
  let bConfidence = (1 - bNovelty)*(fromIntegral . view W.age $ b)
  report $
    agentId a ++ "'s confidence is " ++ printf "%.3f" aConfidence
  report $
    agentId b ++ "'s confidence is " ++ printf "%.3f" bConfidence
  oracle <- use (universe . U.uOracle)
  when (agentId a == oracle) $ do
    report $ oracle ++ " teaches " ++ view W.name b
      ++ " that " ++ O.objectId dObj ++ " is " ++ show aAction
    let (_, _, _, _, b') = W.imprint [p1, pa] aAction b
    assign indirectObjectWain b'
  when (agentId b == oracle) $ do
    report $ oracle ++ " teaches " ++ view W.name a
      ++ " that " ++ O.objectId dObj ++ " is " ++ show bAction
    let (_, _, _, _, a') = W.imprint [p1, pb] bAction a
    assign subject a'

flirt :: StateT Experiment IO ()
flirt = do
  a <- use subject
  b <- use directObjectWain
  babyName <- zoom universe U.genName
  (a':b':_, msgs, aMatingDeltaE, bMatingDeltaE)
    <- liftIO . evalRandIO $ W.mate a b babyName
  if null msgs
    then do
      report $ agentId a ++ " and " ++ agentId b ++ " mated"
      report $ "Contribution to child: " ++
        agentId a ++ "'s share is " ++ show aMatingDeltaE ++ " " ++ 
        agentId b ++ "'s share is " ++ show bMatingDeltaE
      assign subject a'
      assign directObjectWain b'
      recordBirths
      (summary . rMatingDeltaE) += aMatingDeltaE
      (summary . rOtherMatingDeltaE) += bMatingDeltaE
      (summary . rMateCount) += 1
    else mapM_ (report) msgs

recordBirths :: StateT Experiment IO ()
recordBirths = do
  a <- use subject
  (summary.rBirthCount) += length (view W.litter a)

applyFlirtationEffects :: StateT Experiment IO ()
applyFlirtationEffects = do
  deltaE <- use (universe . U.uFlirtingDeltaE)
  report $ "Applying flirtation energy adjustment"
  PW.adjustEnergy subject deltaE rFlirtingDeltaE "flirting" summary
    report
  (summary.rFlirtCount) += 1

updateChildren :: StateT Experiment IO ()
updateChildren = do
  (a:matureChildren) <- W.weanMatureChildren <$> use subject
  assign subject a
  (a':deadChildren) <- W.pruneDeadChildren <$> use subject
  assign subject a'
  assign weanlings (matureChildren ++ deadChildren)
  (summary.rWeanCount) += length matureChildren

killIfTooOld :: StateT Experiment IO ()
killIfTooOld = do
  a <- view W.age <$> use subject
  maxAge <- use (universe . U.uMaxAge)
  when (fromIntegral a > maxAge) $
    PW.adjustEnergy subject (-100) rOldAgeDeltaE "old age"
      summary report

finishRound :: FilePath -> StateT (U.Universe PatternWain) IO ()
finishRound f = do
  xss <- readStats f
  let yss = summarise xss
  printStats yss
  let zs = concat yss
  adjustPopControlDeltaE zs
  cs <- use U.uCheckpoints
  enforceAll zs cs
  clearStats f
  (a, b) <- use U.uAllowedPopulationRange
  checkPopSize (a, b)

adjustPopControlDeltaE
  :: [Stats.Statistic] -> StateT (U.Universe PatternWain) IO ()
adjustPopControlDeltaE xs =
  unless (null xs) $ do
    let (Just average) = Stats.lookup "avg. energy" xs
    let (Just total) = Stats.lookup "total energy" xs
    budget <- use U.uEnergyBudget
    pop <- U.popSize
    let c = idealPopControlDeltaE average total budget pop
    U.writeToLog $ "Current avg. energy = " ++ show average
    U.writeToLog $ "Current total energy = " ++ show total
    U.writeToLog $ "energy budget = " ++ show budget
    U.writeToLog $ "Adjusted pop. control Δe = " ++ show c
    zoom U.uPopControlDeltaE $ putPS c

-- TODO: Make the numbers configurable
idealPopControlDeltaE :: Double -> Double -> Double -> Int -> Double
idealPopControlDeltaE average total budget pop
  | average < 0.8 = min 0.08 $ (budget - total) / (fromIntegral pop)
  | otherwise     = 0.8 - average

totalEnergy :: StateT Experiment IO (Double, Double)
totalEnergy = do
  a <- fmap uiToDouble $ view W.energy <$> use subject
  b <- fmap uiToDouble $ O.objectEnergy <$> use directObject
  c <- fmap uiToDouble $ O.objectEnergy <$> use indirectObject
  d <- W.childEnergy <$> use subject
  e <- O.objectChildEnergy <$> use directObject
  f <- O.objectChildEnergy <$> use indirectObject
  return (a + b + c, d + e + f)

chooseOracle :: StateT (U.Universe PatternWain) IO ()
chooseOracle = do
   xxxxxxxxxxx

printStats :: [[Stats.Statistic]] -> StateT (U.Universe PatternWain) IO ()
printStats = mapM_ f
  where f xs = U.writeToLog $
                 "Summary - " ++ intercalate "," (map pretty xs)


letSubjectReflect
  :: PatternWain -> Response Action -> StateT Experiment IO ()
letSubjectReflect wainBefore r = do
  w <- use subject
  p1 <- O.objectAppearance <$> use directObject
  p2 <- O.objectAppearance <$> use indirectObject
  let energyBefore = view W.energy wainBefore
  let boredomBefore = view W.boredom wainBefore
  let passionBefore = view W.passion wainBefore
  let happinessBefore = W.happiness wainBefore
  energyAfter <- use (subject . W.energy)
  boredomAfter <- use (subject . W.boredom)
  passionAfter <- use (subject . W.passion)
  happinessAfter <- W.happiness <$> use subject
  let deltaH = uiToDouble happinessAfter - uiToDouble happinessBefore
  assign (summary . rDeltaEToReflectOn)
    (uiToDouble energyAfter - uiToDouble energyBefore)
  assign (summary . rDeltaBToReflectOn)
    (uiToDouble boredomAfter - uiToDouble boredomBefore)
  assign (summary . rDeltaPToReflectOn)
    (uiToDouble passionAfter - uiToDouble passionBefore)
  assign (summary . rDeltaHToReflectOn) deltaH
  let (w', err) = W.reflect [p1, p2] r wainBefore w
  assign subject w'
  assign (summary . rErr) err
  when (deltaH < 0) $ do
    b <- use directObject
    c <- use indirectObject
    report $ agentId w ++ "'s choice to " ++ show (_action r) ++ " "
        ++ O.objectId b ++ " " ++ O.objectId c ++ " was a mistake"
    (summary . rMistakeCount) += 1

writeRawStats
  :: String -> FilePath -> [Stats.Statistic]
    -> StateT (U.Universe PatternWain) IO ()
writeRawStats n f xs = do
  liftIO $ createDirectoryIfMissing True (dropFileName f)
  t <- U.currentTime
  liftIO . appendFile f $
    "time=" ++ show t ++ ",agent=" ++ n ++ ',':raw xs ++ "\n"

reportAnyDeaths :: [PatternWain] -> StateT (U.Universe PatternWain) IO ()
reportAnyDeaths ws = mapM_ f ws
  where f w = when (not . isAlive $ w) $
                U.writeToLog
                  (agentId w ++ " dead at age " ++ show (view W.age w))
