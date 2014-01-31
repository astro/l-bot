{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network
import Network.Xmpp
import Network.Xmpp.IM
import Data.Aeson
import Control.Monad.Reader
import Data.Default
import qualified Data.Text as T
import qualified Data.Map as M
import Data.Maybe
import Data.XML.Types
import System.Log.Logger
import Control.Applicative

import Commands
import Config
import Util

type Env r = ReaderT (Message, Session, Config) IO r

main :: IO ()
main = do
  updateGlobalLogger "Pontarius.Xmpp" $ setLevel DEBUG

  eConf <- getConfig

  conf <- case eConf of
    Left e -> error ("Could not load config: " ++ e)
    Right c -> return c

  let (lp, dp, r)  = jidToTexts $ jid conf
      conDetails   = UseHost (T.unpack dp) (PortNumber 5222)
      streamConfig = def { connectionDetails = conDetails }
      sessConfig   = def { sessionStreamConfiguration = streamConfig}

  result <- session (T.unpack dp)
                    (Just (\_ -> ( [scramSha1 (fromJust lp) Nothing (pwd conf)]), r))
                    sessConfig

  sess <- case result of
              Right s -> return s
              Left e -> error $ "XmppFailure: " ++ (show e)

  sendPresence presenceOnline sess

  joinGroupChats sess conf

  forever $ do
      msg <- waitForMessage (isCmd conf) sess
      runReaderT handleCmds (msg, sess, conf)

handleCmds :: Env ()
handleCmds = do
    (m, _, _) <- ask
    let bodies = extractBodies m
    if length bodies > 0
      then do
        let body = head bodies
            (cmdName:args) = T.words body
        case M.lookup (T.tail cmdName) commands of
          Just cmd -> handleCmd cmd args
          Nothing -> return ()
      else return ()

handleCmd :: Command -> [T.Text] -> Env ()
handleCmd cmd args = do
  (m, s, c) <- ask
  r <- lift $ reply cmd args
  let answer
        | messageType m == Chat = answerIM [MessageBody Nothing r] m
        | messageType m == GroupChat && inGroupChat cmd = Just $ answerGroupChat m c r
        | messageType m == GroupChat = Just $ answerGroupChat m c "Only in private chat"
        | otherwise = Nothing
  case answer of
    Just a -> lift $ sendMessage a s >> return ()
    Nothing -> return ()

answerGroupChat :: Message -> Config -> T.Text -> Message
answerGroupChat m c r = do
  let from    = fromJust $ messageFrom m
      room    = jidFromTexts (localpart from) (domainpart from) Nothing
      payload = [body]
      body    = Element { elementName = "body"
                        , elementAttributes = []
                        , elementNodes = [NodeContent $ ContentText r]
                        }
       in Message { messageTo = room
                  , messageFrom = Just (jid c)
                  , messageID = messageID m
                  , messageLangTag = Nothing
                  , messageType = GroupChat
                  , messagePayload = payload
                  }

isCmd :: Config -> Message -> Bool
isCmd c m = messageType m `elem` [GroupChat, Chat]
          && messageFrom m /= Just (jid c) -- ignore own messages
          && all (/= messageFrom m) (map Just $ mucs c) -- ignore own messages
          && resourcepart (fromJust $ messageFrom m) /= Just "1 FreeBOT in da Hood" -- ignore own messages
          && all hasPrefix (extractBodies m)
 where
  hasPrefix s = T.head s == prefix c

extractBodies :: Message -> [T.Text]
extractBodies m = map bodyContent $ imBody $ fromMaybe def (getIM m)
