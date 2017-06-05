{-|
Module      : Lib
Description : Library used to detect spam emails.
Copyright   : (c) Andrzej Pisarek, 2017
-}

module Preprocessing
    ( preprocessEmail
    , vectorizeMail
    , prepareAndSaveDict
    , readDict
    , readTrainingDataset
    ) where

import System.Directory
import Data.List
import Data.Char


-- | Preprocesses email to use it with Machine Learning.
preprocessEmail :: String -> [[Char]]

-- | Preprocesses an email and vectorizes it.
vectorizeMail :: Fractional b => [(a, [Char])] -> String -> [b]

-- | Creates a dictionary of mails' words and saves it to a dict.txt file.
prepareAndSaveDict :: IO ()

-- | Loads a words' dictionary from a previously saved file.
readDict :: IO [(Int, String)]

-- | Loads and prepares an email dataset for Machine Learning spam detection.
readTrainingDataset :: IO [([Double], Integer)]


preprocessEmail email | words email == [] = []
                      | otherwise =
  map toLowerWord .
  filter lengthGreaterThanTwo .
  map filterAlpha .
  filter containsAlpha .
  tail . -- first word is "Subject:" in every email in this dataset, we omit it
  words $ email

vectorizeMail dict mail = map boolToDouble $
  map (\dictWord -> elem dictWord tokens) $
  map snd dict
  where tokens = preprocessEmail mail

prepareAndSaveDict = do
  mails <- allMailsPreprocessed
  let dict = rmdups $ concat mails
  let enumerated = zip [1..] dict
  writeFile dictPath $ intercalate "\n" $ map show enumerated

readDict = do
  text <- readFile dictPath
  return $ map (\word -> read word :: (Int, String)) $ words text

readTrainingDataset = do
  dict <- readDict
  spam <- spamMails
  nonSpam <- nonSpamMails
  let spamLabeled = zip (map (vectorizeMail dict) spam) (repeat 1)
  let nonSpamLabeled = zip (map (vectorizeMail dict) nonSpam) (repeat 0)
  return $ spamLabeled ++ nonSpamLabeled


rootPath :: [Char]
rootPath = "./data/lingspam_public/bare/"

dictPath :: [Char]
dictPath = "./dict.txt"

boolToDouble :: Fractional t => Bool -> t
boolToDouble True = 1.0
boolToDouble False = 0.0

filterAlpha :: [Char] -> [Char]
filterAlpha = filter isAlpha

containsAlpha :: [Char] -> Bool
containsAlpha = any isAlpha

toLowerWord :: [Char] -> [Char]
toLowerWord = map toLower

lengthGreaterThanTwo :: Foldable t => t a -> Bool
lengthGreaterThanTwo word = length word > 2

allPaths :: IO [[Char]]
allPaths = do
  partFoldersNames <- listDirectory rootPath
  let partFolderPaths = map (\folder -> rootPath ++ folder) partFoldersNames
  innerFiles <- mapM listDirectory partFolderPaths
  return $ concat $ zipWith filesWithPaths partFolderPaths innerFiles

filesWithPaths :: [Char] -> [[Char]] -> [[Char]]
filesWithPaths path files = map (\file -> path ++ "/" ++ file) files

spamPaths :: IO [[Char]]
spamPaths = filter (isInfixOf "spmsg") <$> allPaths

nonSpamPaths :: IO [[Char]]
nonSpamPaths = filter (not . isInfixOf "spmsg") <$> allPaths

spamMails :: IO [String]
spamMails = spamPaths >>= mapM readFile

nonSpamMails :: IO [String]
nonSpamMails = nonSpamPaths >>= mapM readFile

allMails :: IO [String]
allMails = (++) <$> spamMails <*> nonSpamMails

allMailsPreprocessed :: IO [[[Char]]]
allMailsPreprocessed = map preprocessEmail <$> allMails

rmdups :: [[Char]] -> [[Char]]
rmdups = map head . filter (\set -> length set >= 10) . group . sort
