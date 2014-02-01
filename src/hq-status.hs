module Main where

import System.Exit
import qualified Data.Text as T
import qualified HQStatus as HQStatus

main = do status <- HQStatus.getHQStatus
          case status of
            Left e -> do
              putStrLn $ "Error: " ++ e
              exitFailure
            Right status' -> do
              putStrLn $ "Open: " ++ show (HQStatus.open status')
              putStrLn $ "Message: " ++ T.unpack (HQStatus.msg status')
              t <- HQStatus.toTime status'
              putStrLn $ "Since: " ++ t
              exitSuccess
