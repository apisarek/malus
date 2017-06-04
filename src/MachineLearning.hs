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
  let x_spam = [sample | (sample, is_spam) <- dataset, is_spam == 1]
  let x_non_spam = [sample | (sample, is_spam) <- dataset, is_spam == 0]
  let x_spam_T = transpose x_spam
  let x_non_spam_T = transpose x_non_spam
  let length_spam = fromIntegral $ length x_spam
  let length_non_spam = fromIntegral $ length x_non_spam
  let spam_ratio = length_spam / (length_spam + length_non_spam)
  let non_spam_ratio = 1 - spam_ratio
  let occurencesSpamRatio = transformToOccurenceRatio length_spam x_spam_T
  let occurencesNonSpamRatio = transformToOccurenceRatio length_non_spam x_non_spam_T
  let class_ratios = [spam_ratio, non_spam_ratio]
  let things_to_save = [occurencesSpamRatio, occurencesNonSpamRatio, class_ratios]
  writeFile modelPath $ intercalate "\n" $ map show things_to_save

loadEvaluateNBModel = do
  evaluateOnVectorized <- readNBModel
  dict <- readDict
  return $ evaluateOnVectorized . (vectorizeMail dict)


modelPath = "./model.txt"

transformToOccurenceRatio classLength = map $ (/ classLength) . (+1) . sum

createEvaluate classRatio occurrenceRatios sample =
  (*) classRatio $
  product $
  zipWith (\ratio exists -> if exists == 1 then ratio else 1 - ratio) occurrenceRatios sample

readNBModel = do
  model <- readFile modelPath
  let [occurrencesSpamRatio, occurrencesNonSpamRatio, classRatios] = map (\word -> read word :: [Double]) $ words model
  let [spamRatio, nonSpamRatio] = classRatios
  let evaluateSpam = createEvaluate spamRatio occurrencesSpamRatio
  let evaluateNonSpam = createEvaluate nonSpamRatio occurrencesNonSpamRatio
  return $ (>) <$> evaluateSpam <*> evaluateNonSpam
