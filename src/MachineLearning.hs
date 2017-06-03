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

trainAndSaveNBModel :: IO ()
trainAndSaveNBModel = do
  dataset <- readTrainingDataset
  let x_spam = [sample | (sample, spam) <- dataset, spam == 1]
  let x_non_spam = [sample | (sample, spam) <- dataset, spam == 0]
  let x_spam_T = transpose x_spam
  let x_non_spam_T = transpose x_non_spam
  let length_spam = fromIntegral $ length x_spam
  let length_non_spam = fromIntegral $ length x_non_spam
  let spam_ratio = length_spam / (length_spam + length_non_spam)
  let non_spam_ratio = 1 - spam_ratio
  let occurrences_spam = map sum x_spam_T
  let occurrences_non_spam = map sum x_non_spam_T
  let occurrences_spam_smoothed = map (+1) occurrences_spam
  let occurrences_non_spam_smoothed = map (+1) occurrences_non_spam
  let occurrences_spam_smoothed_ratio = map (/ length_spam ) occurrences_spam_smoothed
  let occurrences_non_spam_smoothed_ratio = map (/  length_non_spam ) occurrences_non_spam_smoothed
  let class_ratios = [spam_ratio, non_spam_ratio]
  let things_to_save = [occurrences_spam_smoothed_ratio, occurrences_non_spam_smoothed_ratio, class_ratios]
  writeFile "./model.txt" $ intercalate "\n" $ map show things_to_save


readNBModel = do
  model <- readFile "./model.txt"
  let [occurrences_spam_smoothed_ratio, occurrences_non_spam_smoothed_ratio, class_ratios] = map (\word -> read word :: [Double]) $ words model
  let [spam_ratio, non_spam_ratio] = class_ratios
  let evaluate_spam ratios sample = (*) spam_ratio $ product $ zipWith (\ratio exists -> if exists == 1 then ratio else 1 - ratio) ratios sample
  let evaluate_non_spam ratios sample = (*) non_spam_ratio $ product $ zipWith (\ratio exists -> if exists == 1 then ratio else 1 - ratio) ratios sample
  let evaluate sample = evaluate_spam occurrences_spam_smoothed_ratio sample > evaluate_non_spam occurrences_non_spam_smoothed_ratio sample
  return evaluate

loadEvaluateNBModel :: IO (String -> Bool)
loadEvaluateNBModel = do
  evaluateOnVectorized <- readNBModel
  dict <- readDict
  return $ evaluateOnVectorized . (vectorizeMail dict)


