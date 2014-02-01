{-# LANGUAGE OverloadedStrings #-}

module Commands (
    Command(..)
  , commands
  ) where

import Network
import Network.HTTP
import Data.Aeson
import Control.Applicative
import Control.Monad
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.ByteString.Lazy.Char8 as BC
import System.Time

data Command = Command
  { help        :: T.Text
  , reply       :: [T.Text] -> IO T.Text
  , inGroupChat :: Bool
  , showInInfo  :: Bool
  }

data HQStatus = HQStatus { _open      :: !Bool
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

getHQStatus :: [T.Text] -> IO T.Text
getHQStatus _ = do
  src <- getResponseBody =<< simpleHTTP (getRequest "http://www.hq.c3d2.de/spaceapi.json")
  let status = eitherDecode $ BC.pack src
  case status of
    Left err -> return $ T.pack $
                "Error: " ++ err
    Right st -> do
      t <- toTime st
      return $ T.concat [msg st, " (", T.pack t, ")"]
 where
  toTime st = do
    let ut = lastchange st
    lTime <- toCalendarTime $ TOD ut 0
    return $ calendarTimeToString lTime

info :: [T.Text] -> IO T.Text
info _ = do
  let about = "I am λ-Bot - written in exceptionally bad haskell"
  return $
    T.unlines $ about : [infoMsg cname h i s | (cname, Command h _ i s) <- M.toList commands, s ]
 where
  infoMsg n h i s = T.concat [n, ": ", h, if not i then " -- in private chat only" else ""]

commands :: M.Map T.Text Command
commands = M.fromList
  [ ("ping",   Command "Answers with pong" (\_ -> return "pong") True True)
  , ("calc",   Command "Simple calculator -- not implented (yet)" (\_ -> return "Not implemented") False True)
  , ("status", Command "Shows GCHQ status" getHQStatus True True)
  , ("info",   Command "" info True False)
  ]
