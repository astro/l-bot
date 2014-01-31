{-# LANGUAGE OverloadedStrings #-}

module Util (
  joinGroupChats
  ) where

import Network.Xmpp
import qualified Data.Text as T
import Data.XML.Types
import Control.Monad

import Config

joinGroupChats :: Session -> Config -> IO ()
joinGroupChats s conf = mapM_ joinGroupChat (mucs conf)
 where
  joinGroupChat muc = do
    let p = presence { presenceFrom = Just (jid conf)
                    , presenceTo = Just muc
                    , presencePayload = [mucNamespaceElem]
                    }
    sendPresence p s
  mucNamespaceElem = Element { elementName = "x"
                             , elementAttributes = [(xmlns, [mucns])]
                             , elementNodes = [noHist]
                             }
  xmlns = Name "xmlns" Nothing Nothing
  mucns = ContentText "http://jabber.org/protocol/muc"
  noHist = NodeElement $ Element { elementName = "history"
                                 , elementAttributes = [(maxchars, [ContentText "0"])]
                                 , elementNodes = []
                                 }
  maxchars = Name "maxchars" Nothing Nothing
