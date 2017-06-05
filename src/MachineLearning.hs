{-|
Module      : MachineLearning
Description : Implemented machine learning algorithms.
Copyright   : (c) Andrzej Pisarek, 2017
-}

module MachineLearning
    ( trainAndSaveNBModel
    , loadEvaluateNBModel
    ) where

import Preprocessing
import Data.List


-- | Train model using Naive Bayes algorithm and save it to a model.txt file for a future use.
trainAndSaveNBModel :: IO ()

-- | Load saved model from model.txt file and use it to evaluate mails.
loadEvaluateNBModel :: IO (String -> Bool)


trainAndSaveNBModel = do
  dataset <- readTrainingDataset
  let xSpam = [sample | (sample, isSpam) <- dataset, isSpam == 1]
  let xNonSpam = [sample | (sample, isSpam) <- dataset, isSpam == 0]
  let lengthSpam = fromIntegral $ length xSpam
  let lengthNonSpam = fromIntegral $ length xNonSpam
  let spamRatio = lengthSpam / (lengthSpam + lengthNonSpam)
  let nonSpamRatio = 1 - spamRatio
  let occurrencesSpamRatio = transformToOccurrenceRatio lengthSpam $ transpose xSpam
  let occurrencesNonSpamRatio = transformToOccurrenceRatio lengthNonSpam $ transpose xNonSpam
  let classRatios = [spamRatio, nonSpamRatio]
  let thingsToSave = [occurrencesSpamRatio, occurrencesNonSpamRatio, classRatios]
  writeFile modelPath $ intercalate "\n" $ map show thingsToSave

loadEvaluateNBModel = do
  evaluateOnVectorized <- readNBModel
  dict <- readDict
  return $ evaluateOnVectorized . (vectorizeMail dict)


modelPath :: String
modelPath = "./model.txt"

transformToOccurrenceRatio :: (Foldable t, Fractional b) => b -> [t b] -> [b]
transformToOccurrenceRatio classLength = map $ (/ classLength) . (+1) . sum

createEvaluate :: (Num a1, Num a, Eq a1) => a -> [a] -> [a1] -> a
createEvaluate classRatio occurrenceRatios sample =
  (*) classRatio $ product $ zipWith ratioExistence occurrenceRatios sample

ratioExistence :: (Num a1, Num a, Eq a) => a1 -> a -> a1
ratioExistence ratio exists = if exists == 1 then ratio else 1 - ratio

readNBModel :: IO ([Double] -> Bool)
readNBModel = do
  model <- readFile modelPath
  let [occurrencesSpamRatio, occurrencesNonSpamRatio, classRatios] =
        map (\word -> read word :: [Double]) $ words model
  let [spamRatio, nonSpamRatio] = classRatios
  let evaluateSpam = createEvaluate spamRatio occurrencesSpamRatio
  let evaluateNonSpam = createEvaluate nonSpamRatio occurrencesNonSpamRatio
  return $ (>) <$> evaluateSpam <*> evaluateNonSpam
