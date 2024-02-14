{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <&>" #-}
module Main (main) where

import RIO
import Prelude (putStrLn)
import Server(startServer)
import Types(AppConfig(..))
import qualified DB.Transaction as DBT
import DB.Migration (autoMigrate)
import GHC.IO.Handle as H



getDBPool =
    let
      connectionString = "host=db port=5432 user=postgres password=postgres dbname=riocabal connect_timeout=10"
      poolSize = 4
    in DBT.loadPool connectionString poolSize

prepareFileIO :: IO ()
prepareFileIO =
  openFile "example.txt" WriteMode
  >>= \hFile -> H.hDuplicateTo stdout hFile

main :: IO ()
main =
  prepareFileIO
  >>  putStrLn "Hello, Haskell from IO"
  >>  logOptionsHandle stdout False
  >>= pure
  . setLogUseTime True
  . setLogMinLevel LevelDebug
  . setLogUseLoc True
  . setLogUseColor True

  >>= \logOptions -> withLogFunc logOptions


  $ \logFunc -> getDBPool
  >>= \dbPool -> autoMigrate dbPool
  >>
    let
      appConfig
            = AppConfig
            { appLogFunc = logFunc
            , appPort = 8080
            , appDBPool = dbPool -- todo
            }

    in runRIO appConfig
    $ logInfo "Hello, Haskell from RIO"
    >> startServer appConfig
