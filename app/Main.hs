{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TemplateHaskell     #-}

module Main where

import           Control.Exception
import           Network.Bugsnag
import           System.Environment
import           Data.Aeson
import           Data.ByteString.Lazy       as BL
import           Data.List.NonEmpty
import           Data.Text
import qualified Data.Text                  as T
import           Language.Haskell.TH.Syntax
import           Network.HTTP.Client.TLS
import           Network.HTTP.Client
import qualified Network.HTTP.Types.Method  as NHTM
import qualified Network.HTTP.Types.Status  as NHTS
import qualified Network.HTTP.Types.URI     as NHTU
import qualified Network.URI                as URI

appKey :: IO Text
appKey = T.pack <$> getEnv "BUGSNAG_APP_KEY"

testEvent :: BugsnagEvent
testEvent = BugsnagEvent
  [testException]
  (Just [testThread])
  (Just "context")
  (Just "groupingHash")
  (Just "warning")
  testUser
  testApp
  testDevice
  Nothing

testThread :: BugsnagThread
testThread = BugsnagThread
  "2"
  "Main"
  (testStackFrame :| [])

testStackFrame :: BugsnagStackFrame
testStackFrame = BugsnagStackFrame "Main.hs" "myReq" 88 (Just 0) True mempty

testException :: BugsnagException
testException = BugsnagException "errorClass" "There was an error!" [testStackFrame]

testUser :: BugsnagUser
testUser = BugsnagUser (Just "10") (Just "Ben") (Just "ben@ben.com")

testApp :: BugsnagApp
testApp = BugsnagApp (Just "1.0") (Just "development") Nothing

testDevice :: BugsnagDevice
testDevice = BugsnagDevice (Just "1.0") (Just "web.io.com")

testRequest :: BugsnagApiKey -> BugsnagRequest
testRequest key = BugsnagRequest key [testEvent]

data MyException = MyException
  { exRow     :: !Int
  , exColumn  :: !Int
  , exName    :: !Text
  , exMessage :: !Text
  , exFile    :: !Text
  }

instance BugsnagError MyException where
  errorRow = exRow
  errorColumn = Just . exColumn
  errorName = exName
  errorMessage = Just . exMessage
  errorFile = Just . exFile

deriving instance Show MyException

instance Exception MyException

main :: IO ()
main = do
  -- ??? How to get location to show up in proper position
  undefined `catch` \(e :: SomeException) -> do
    print $(qLocation >>= liftLoc)
  key <- appKey
  req <- bugsnagHttpRequest (testRequest (BugsnagApiKey key))
  mgr <- newManager tlsManagerSettings
  resp <- httpLbs req mgr
  print resp
