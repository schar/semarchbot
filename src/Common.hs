{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Common (getTWInfoFromEnv) where

import Web.Twitter.Conduit

import Control.Applicative
import Control.Lens
import Credentials
import qualified Data.ByteString.Char8 as S8
import qualified Data.CaseInsensitive as CI
import qualified Data.Map as M
import Network.HTTP.Conduit
import qualified Network.URI as URI
import System.Environment

getOAuthTokens :: (OAuth, Credential)
getOAuthTokens =
  let oauth =
        twitterOAuth
          {oauthConsumerKey = apiKey, oauthConsumerSecret = apiKeySecret}
      cred =
        Credential
          [ ("oauth_token", accessToken)
          , ("oauth_token_secret", accessTokenSecret)
          ]
   in (oauth, cred)

getProxyEnv :: IO (Maybe Proxy)
getProxyEnv = do
  env <- M.fromList . over (mapped . _1) CI.mk <$> getEnvironment
  let u =
        M.lookup "https_proxy" env <|> M.lookup "http_proxy" env <|>
        M.lookup "proxy" env >>=
        URI.parseURI >>=
        URI.uriAuthority
  return $
    Proxy <$> (S8.pack . URI.uriRegName <$> u) <*>
    (parsePort . URI.uriPort <$> u)
  where
    parsePort :: String -> Int
    parsePort [] = 8080
    parsePort (':':xs) = read xs
    parsePort xs = error $ "port number parse failed " ++ xs

getTWInfoFromEnv :: IO TWInfo
getTWInfoFromEnv = do
  pr <- getProxyEnv
  let (oa, cred) = getOAuthTokens
  return $ (setCredential oa cred def) {twProxy = pr}
