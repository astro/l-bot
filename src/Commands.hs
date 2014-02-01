{-# LANGUAGE OverloadedStrings #-}

module Commands (
    Command(..)
  , commands
  ) where

import Data.Aeson
import qualified Data.Text as T
import qualified Data.Map as M
import HQStatus (getHQStatusString)

data Command = Command
  { help        :: T.Text
  , reply       :: [T.Text] -> IO T.Text
  , inGroupChat :: Bool
  , showInInfo  :: Bool
  }

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
  , ("status", Command "Shows GCHQ status" (const getHQStatusString) True True)
  , ("info",   Command "" info True False)
  ]
