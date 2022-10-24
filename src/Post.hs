{-# LANGUAGE OverloadedStrings #-}

module Post (post) where

import Common
import Web.Twitter.Conduit

import qualified Data.Text as T
import qualified Data.Text.IO as T

post :: T.Text -> IO ()
post status = do
  T.putStrLn $ "Post message: " <> status
  twInfo <- getTWInfoFromEnv
  mgr <- newManager tlsManagerSettings
  res <- call twInfo mgr $ statusesUpdate status
  appendFile "log.log" (show res)
