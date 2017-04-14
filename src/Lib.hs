module Lib
    ( someFunc
    , allPaths
    , spamPaths
    , preprocessEmail
    , spamMails
    , allMails
    , rmdups
    , prepareAndSaveDict
    , readDict
    , vectorizeMail
    , readTrainingDataset
    ) where

import System.Directory
import Data.List
import Data.Char

someFunc :: IO ()
someFunc = putStrLn "someFunc"

rootPath = "./data/lingspam_public/bare/"

allPaths = do
  partFoldersNames <- listDirectory rootPath
  let partFolderPaths = map (\folder -> rootPath ++ folder) partFoldersNames
  innerFiles <- mapM listDirectory partFolderPaths
  return $ concat $ zipWith (\path files -> map (\file -> path ++ "/" ++ file) files) partFolderPaths innerFiles

spamPaths = do
  paths <- allPaths
  return $ filter (\path -> isInfixOf "spmsg" path) paths

nonSpamPaths = do
  paths <- allPaths
  return $ filter (\path -> not $ isInfixOf "spmsg" path) paths

spamMails = do
  paths <- spamPaths
  mapM readFile paths

nonSpamMails = do
  paths <- nonSpamPaths
  mapM readFile paths

allMails = do
  spam <- spamMails
  nonSpam <- nonSpamMails
  return $ (spam ++ nonSpam)

allMailsPreprocessed = do
  mails <- allMails
  return $ map preprocessEmail mails



filterAlpha = filter isAlpha
containsAlpha = any isAlpha
toLowerWord = map toLower
lengthGreaterThanTwo word = length word > 2

rmdups :: (Ord a) => [a] -> [a]
rmdups = map head . filter (\set -> length set >= 10) . group . sort

prepareAndSaveDict = do
  mails <- allMailsPreprocessed
  let dict = rmdups $ concat mails
  let enumerated = zip [1..] dict
  writeFile "./dict.txt" $ intercalate "\n" $ map show enumerated

readDict = do
  text <- readFile "./dict.txt"
  return $ map (\word -> read word :: (Int, String)) $ words text

readTrainingDataset = do
  dict <- readDict
  spam <- spamMails
  nonSpam <- nonSpamMails
  let spamLabeled = zip (map (vectorizeMail dict) spam) (repeat 1)
  let nonSpamLabeled = zip (map (vectorizeMail dict) nonSpam) (repeat 0)
  return $ spamLabeled ++ nonSpamLabeled


boolToDouble True = 1.0
boolToDouble False = 0.0

vectorizeMail dict mail = map boolToDouble $ map (\dictWord -> elem dictWord tokens) $ map snd dict
  where tokens = preprocessEmail mail



preprocessEmail email =
  map toLowerWord .
  filter lengthGreaterThanTwo .
  map filterAlpha .
  filter containsAlpha .
  tail . -- first word is "Subject:" in every email in this dataset, so we omit it
  words $ email