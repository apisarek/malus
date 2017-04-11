module Lib
    ( someFunc
    , allPaths
    ) where

import System.Directory

someFunc :: IO ()
someFunc = putStrLn "someFunc"

rootPath = "./data/lingspam_public/bare/"

allPaths = do
  partFoldersNames <- listDirectory rootPath
  let partFolderPaths = map (\folder -> rootPath ++ folder) partFoldersNames
  innerFiles <- mapM listDirectory partFolderPaths
  return $ concat $ zipWith (\path files -> map (\file -> path ++ "/" ++ file) files) partFolderPaths innerFiles

