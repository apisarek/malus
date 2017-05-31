{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-|
Module      : Rest
Description : Simple REST server for our spam detection library.
Copyright   : (c) Bartosz Sumper, 2017

Rest server listens for POST requests on "/spam".
POST body should be: { "content": "yourMail" }.
Returns true if the given mail is a spam mail.
-}
module Rest
    ( startServer
    ) where

import Web.Scotty
import Data.Aeson (FromJSON)
import GHC.Generics
import Network.HTTP.Types.Status
import Preprocessing (allPaths, checkEmail)

data Email = Email {content :: String} deriving (Show, Generic)
instance FromJSON Email

-- | Starts REST server on port 3000.
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
