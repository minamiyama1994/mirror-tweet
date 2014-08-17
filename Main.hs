{-# OPTIONS -Wall -fsimpl-tick-factor=1024 #-}
{-# LANGUAGE OverloadedStrings #-}
import qualified Web.Twitter.Conduit as WTC
import qualified Web.Authenticate.OAuth as WAO
import qualified Control.Monad.Logger as CML
import qualified System.Environment as SE
import qualified Control.Lens as CL
import qualified Control.Monad.IO.Class as CMIC
import qualified Data.Conduit as DC
import qualified Data.Conduit.List as DCL
import qualified Data.ByteString.Char8 as DBC
import qualified Data.Text as DT
import qualified Data.Monoid as DM

tokens :: String -> String -> WAO.OAuth
tokens ck cs = WTC.twitterOAuth
  {
    WAO.oauthConsumerKey = DBC.pack ck ,
    WAO.oauthConsumerSecret = DBC.pack cs
  }

credential :: String -> String -> WAO.Credential
credential t ts = WAO.Credential
  [
    ( "oauth_token" , DBC.pack t ) ,
    ( "oauth_token_secret" , DBC.pack ts )
  ]

twInfo :: String -> String -> String -> String -> WTC.TWInfo
twInfo ck cs t ts = WTC.setCredential ( tokens ck cs ) ( credential t ts ) WAO.def

mirroring sn ( WTC.SStatus status ) = do
  if status CL.^. WTC.statusUser CL.^. WTC.userScreenName == DT.pack sn
    then do
      _ <- WTC.call $ WTC.update $ status CL.^. WTC.statusText
      return ( )
    else return ( )
mirroring _ _ = return ( )

main :: IO ( )
main = do
  ck <- SE.getEnv "YOUR_CONSUMER_KEY"
  cs <- SE.getEnv "YOUR_CONSUMER_SECRET"
  at <- SE.getEnv "YOUR_ACCESS_TOKEN"
  ats <- SE.getEnv "YOUR_ACCESS_TOKEN_SECRET"
  sn <- SE.getEnv "MIRRORING_ACCOUNT_SCREEN_NAME"
  CML.runNoLoggingT . WTC.runTW ( twInfo ck cs at ats ) $ do
    userId <- WTC.call $ WTC.usersShow $ WTC.ScreenNameParam sn
    src <- WTC.stream $ WTC.statusesFilterByFollow [ userId CL.^. WTC.userId  ]
    src DC.$$+- DCL.mapM_ ( CL.^! CL.act ( mirroring sn ) )

