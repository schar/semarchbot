{-# LANGUAGE OverloadedStrings #-}

module Post where

import           Common
import           Web.Twitter.Conduit hiding (map)

import           Control.Applicative
import           Data.Monoid
import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import           System.Environment

post :: T.Text -> IO ()
post status = do
    T.putStrLn $ "Post message: " <> status
    twInfo <- getTWInfoFromEnv
    mgr <- newManager tlsManagerSettings
    res <- call twInfo mgr $ update status
    writeFile "last_twetes.log" (show res)
