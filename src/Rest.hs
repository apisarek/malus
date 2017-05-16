{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Rest
    ( startServer
    ) where

import Web.Scotty
import Data.Aeson (FromJSON)
import GHC.Generics
import Network.HTTP.Types.Status
import Lib (allPaths, checkEmail)

data Email = Email {content :: String} deriving (Show, Generic)
instance FromJSON Email

startServer :: IO ()
startServer = scotty 3000 routes

routes :: ScottyM ()
routes = do
  post "/spam" checkSpamEmail
  post "/spam" checkSpamError

checkSpamEmail :: ActionM ()
checkSpamEmail = do
  mail <- (jsonData :: ActionM Email) `rescue` (const next)
  json $ checkEmail $ content mail

checkSpamError :: ActionM ()
checkSpamError = do
  text "Body should be: { \"content\": \"yourMail\" }"
  status badRequest400
