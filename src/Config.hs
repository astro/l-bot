
{-# LANGUAGE OverloadedStrings #-}

module Config (
    Config(..)
  , getConfig
  ) where

import Network.Xmpp
import Data.Aeson
import qualified Data.Text as T
import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Lazy as B

import Commands

data Config = ConfigJSON
  { jid_    :: !String
  , pwd_    :: !T.Text
  , prefix_ :: Char
  , mucs_   :: ![String]
  } | Config
  { jid    :: Jid
  , pwd    :: !T.Text
  , prefix :: Char
  , mucs   :: ![Jid]
  }
  deriving (Show)

instance FromJSON Config where
  parseJSON (Object v) =
    ConfigJSON <$> v .: "jid"
               <*> v .: "pwd"
               <*> v .: "prefix"
               <*> v .: "mucs"
  parseJSON _ = mzero


configFile :: FilePath
configFile = ".l-bot.conf"

getConfig :: IO (Either String Config)
getConfig = do
  configJSON <- eitherDecode <$> B.readFile configFile
  case configJSON of
    Left e -> return $ Left e
    Right c -> return $ Right Config { jid = parseJid (jid_ c)
                               , pwd = pwd_ c
                               , prefix = prefix_ c
                               , mucs = map parseJid (mucs_ c)
                               }
