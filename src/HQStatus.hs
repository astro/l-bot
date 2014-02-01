{-# LANGUAGE OverloadedStrings #-}

module HQStatus where

import Control.Applicative
import Control.Monad
import Network
import Network.HTTP
import Data.Aeson
import System.Time
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as BC

data HQStatus = HQStatus { open      :: !Bool
                         , msg        :: !T.Text
                         , lastchange :: Integer
                         }

-- | FIXME: Version?
instance FromJSON HQStatus where
  parseJSON (Object v) =
    HQStatus <$> ((v .: "state") >>= (.: "open"))
             <*> ((v .: "state") >>= (.: "message"))
             <*> ((v .: "state") >>= (.: "lastchange"))
  parseJSON _ = mzero

spaceApiUrl :: String
spaceApiUrl = "http://www.hq.c3d2.de/spaceapi.json"

getHQStatus :: IO (Either String HQStatus)
getHQStatus =
  simpleHTTP (getRequest spaceApiUrl) >>=
  getResponseBody >>=
  return . eitherDecode . BC.pack

getHQStatusString :: IO T.Text
getHQStatusString = do
  status <- getHQStatus
  case status of
    Left err -> return $ T.pack $
                "Error: " ++ err
    Right st -> do
      t <- toTime st
      return $ T.concat [msg st, " (", T.pack t, ")"]

toTime :: HQStatus -> IO String
toTime st = do
  let ut = lastchange st
  lTime <- toCalendarTime $ TOD ut 0
  return $ calendarTimeToString lTime
