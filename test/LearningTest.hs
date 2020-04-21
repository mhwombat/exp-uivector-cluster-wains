------------------------------------------------------------------------
-- |
-- Module      :  LearningTest
-- Copyright   :  (c) Amy de Buitléir 2013-2016
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Verify that a wain can learn.
--
------------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import ALife.Creatur.Wain
import ALife.Creatur.Wain.BrainInternal (classifier, predictor,
  makeBrain, scenarioReport, responseReport, decisionReport,
  decisionQuality)
import ALife.Creatur.Wain.Classifier (buildClassifier)
import ALife.Creatur.Wain.GeneticSOMInternal (LearningParams(..),
  schemaQuality)
import ALife.Creatur.Wain.Muser (makeMuser)
import ALife.Creatur.Wain.UIVector.Cluster.Action (Action(..))
import ALife.Creatur.Wain.UIVector.Cluster.Experiment (PatternWain)
import ALife.Creatur.Wain.UIVector.Object (Object(..), objectId,
  objectAppearance)
import ALife.Creatur.Wain.UIVector.Pattern (Pattern)
import qualified ALife.Creatur.Wain.UIVector.Wain as PW
import ALife.Creatur.Wain.UIVector.Tweaker (PatternTweaker(..))
import ALife.Creatur.Wain.UnitInterval (UIDouble)
import ALife.Creatur.Wain.PlusMinusOne (doubleToPM1)
import ALife.Creatur.Wain.Predictor (buildPredictor)
import ALife.Creatur.Wain.Response (action, outcomes)
import ALife.Creatur.Wain.Statistics (stats)
import ALife.Creatur.Wain.UnitInterval (uiToDouble)
import ALife.Creatur.Wain.Weights (makeWeights)
import Control.Lens
import Control.Monad (foldM)
import Data.List (minimumBy)
import Data.List.Split (splitOn)
import Data.Ord (comparing)
import Text.Read (readEither)

reward :: Double
reward = 0.1

runAction :: Action -> Object Action -> PatternWain -> PatternWain
runAction a obj w =
  if a == correctAnswer obj
    then wCorrect
    else wIncorrect
  where (wCorrect, _) = adjustEnergy reward w
        (wIncorrect, _) = adjustEnergy (-reward) w
        
testWain :: PatternWain
testWain = w'
  where wName = "Fred"
        wAppearance = [0, 0]
        Right wBrain = makeBrain wClassifier wMuser wPredictor wHappinessWeights 1 32 wIos wRds
        wDevotion = 0.1
        wAgeOfMaturity = 100
        wPassionDelta = 0
        wBoredomDelta = 0
        wClassifier = buildClassifier ec wCSize 0.1 (PatternTweaker wTw)
        wCSize = 500
        wTw = makeWeights [1, 1]
        wMuser = makeMuser [0, 0, 0, 0] 1
        wIos = [doubleToPM1 reward, 0, 0, 0]
        wRds = [0.1, 0, 0, 0]
        wPredictor = buildPredictor ep (wCSize*15) 0.1
        wHappinessWeights = makeWeights [1, 0, 0, 0]
        ec = LearningParams 1 0.001 500
        ep = LearningParams 1 0.001 500
        w = buildWainAndGenerateGenome wName wAppearance wBrain
              wDevotion wAgeOfMaturity wPassionDelta wBoredomDelta
        (w', _) = adjustEnergy 0.5 w

putHtml :: String -> IO ()
putHtml s = putStr s 

putHtmlLn :: String -> IO ()
putHtmlLn s = putStrLn $ s ++ "<br/>"

tryOne :: PatternWain -> Object Action -> IO (PatternWain)
tryOne w obj = do
  putHtmlLn $ "-----<br/>"
  putHtmlLn $ "stats=" ++ show (stats w)
  -- putHtmlLn "Initial classifier models:"
  -- IW.describeClassifierModels w
  -- putHtmlLn "Initial prediction models"
  -- IW.describePredictorModels w
  let (lds, sps, rplos, aos, r, wainAfterDecision) = chooseAction [objectAppearance obj] w
  let (cBMU, _) = minimumBy (comparing snd) . head $ lds
  mapM_ putHtmlLn $ scenarioReport sps
  mapM_ putHtmlLn $ responseReport rplos
  mapM_ putHtmlLn $ decisionReport aos
  putHtmlLn $ "DEBUG classifier SQ=" ++ show (schemaQuality . view (brain . classifier) $ wainAfterDecision)
  mapM_ putHtmlLn $ PW.describeClassifierModels wainAfterDecision
  mapM_ putHtmlLn $ PW.describePredictorModels wainAfterDecision
  let a = view action r
  putStrLn $ "DEBUG obj=" ++ show obj ++ " expecting " ++ show (correctAnswer obj)
  putHtmlLn $ "Wain sees " ++ objectId obj ++ ", classifies it as "
    ++ show cBMU ++ " and chooses to " ++ show a
    ++ " predicting the outcomes " ++ show (view outcomes r)
  putHtmlLn $ show (objectAppearance obj)
  let wainRewarded = runAction a obj wainAfterDecision
  let deltaH = uiToDouble (happiness wainRewarded) - uiToDouble (happiness w)
  putHtmlLn $ "Δh=" ++ show deltaH
  putHtmlLn $ "condition before=" ++ show (condition w) ++ " after=" ++ show (condition wainRewarded)
  putHtmlLn $ "happiness before=" ++ show (happiness w) ++ " after=" ++ show (happiness wainRewarded)
  let b = correctAnswer obj
  putStrLn $ "DEBUG a=" ++ show a ++ " b" ++ show b ++ " equals? " ++ show (a==b)
  putHtml $ "Choosing to " ++ show a ++ " in response to " ++ objectId obj
  if a == b
    then putHtmlLn " was correct"
    else putHtmlLn $ " was wrong; expected " ++ show b
  let (wainAfterReflection, err) = reflect [objectAppearance obj] r w wainRewarded
  putHtmlLn $ "err=" ++ show err
  -- keep the wain's energy constant
  let restorationEnergy = uiToDouble (view energy w) - uiToDouble (view energy wainRewarded)
  -- keep the wain's boredom constant
  let restorationBoredom = uiToDouble (view boredom w) - uiToDouble (view boredom wainRewarded)
  let (wainPartiallyRestored, _) = adjustEnergy restorationEnergy wainAfterReflection
  let (wainFinal, _) = adjustBoredom restorationBoredom wainPartiallyRestored
  -- putHtmlLn "Final classifier models"
  -- IW.describeClassifierModels wainFinal
  -- putHtmlLn "Final prediction models"
  -- IW.describePredictorModels wainFinal
  putHtmlLn $ "classifier SQ=" ++ show (schemaQuality . view (brain . classifier) $ w)
  putHtmlLn $ "predictor SQ=" ++ show (schemaQuality . view (brain . predictor) $ w)
  putHtmlLn $ "DQ=" ++ show (decisionQuality . view brain $ w)
  return wainFinal

correctAnswer :: Object Action -> Action
correctAnswer (PObject _ s) = read $ "Cooperate_" ++ [head s]
correctAnswer _ = error "wain!"

-- describeClassifierModels :: PatternWain -> IO ()
-- describeClassifierModels w = mapM_ (putHtml . f) ms >> putHtmlLn ""
--   where ms = M.toList . modelMap . view (brain . classifier) $ w
--         f (l, r) = show l ++ ": <img src='data:image/png;base64,"
--                      ++ base64encode r ++ "'/>"

-- describePredictorModels :: PatternWain -> IO ()
-- describePredictorModels w = describePredictorModels' ms
--   where ms = M.toList . modelMap . view (brain . predictor) $ w

-- describePredictorModels' :: [(Label, Response Action)] -> IO ()
-- describePredictorModels' [] = return ()
-- describePredictorModels' xs = do
--   putHtmlLn $ concatMap f ys
--   describePredictorModels' zs
--   where (ys, zs) = splitAt 4 xs
--         f (l, r) = show l ++ ": " ++ pretty r ++ " "

dataFile :: FilePath
dataFile = "/home/eamybut/néal/spaeth/amy2.csv"

toObject :: (String, [UIDouble]) -> Object Action
toObject (s, xs) = PObject xs s

fromCSV :: String -> ([String], [(String, [UIDouble])])
fromCSV xss = extractValues . tokenise $ xss

tokenise :: String -> [[String]]
tokenise = map (splitOn ",") . lines

extractValues :: [[String]] -> ([String], [(String, [UIDouble])])
extractValues xss =
  if null xss
    then error "no data"
    else (headings, values)
  where (headings:xs) = xss
        values = map parseLine xs

parseLine :: [String] -> (String, Pattern)
parseLine (x:xs) = (x, map safeRead xs)
parseLine [] = ("???",[])

safeRead :: String -> UIDouble
safeRead s = case readEither s of
               Left msg -> error $
                            "Unable to parse '" ++ s ++ "': " ++ msg
               Right x  -> x

main :: IO ()
main = do
  (_,xss) <- fmap fromCSV $ readFile dataFile
  _ <- foldM tryOne testWain (map toObject xss)
  putHtmlLn "test complete"

