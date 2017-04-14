module Lib
    ( someFunc
    , allPaths
    , spamPaths
    , preprocessEmail
    , spamMails
    , allMails
    , rmdups
    , prepareDict
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
  return $ map preprocessEmail (spam ++ nonSpam)



filterAlpha = filter isAlpha
containsAlpha = any isAlpha
toLowerWord = map toLower
lengthGreaterThanTwo word = length word > 2

rmdups :: (Ord a) => [a] -> [a]
rmdups = map head . filter (\set -> length set >= 10) . group . sort

prepareDict = do
  mails <- allMails
  let dict = rmdups $ concat mails
  let enumerated = zip [1..] dict
  writeFile "./dict.txt" $ intercalate "\n" $ map show enumerated


preprocessEmail email =
  map toLowerWord .
  filter lengthGreaterThanTwo .
  map filterAlpha .
  filter containsAlpha .
  tail . -- first word is "Subject:" in every email in this dataset, so we omit it
  words $ email