module Network.Bugsnag.Client where

import Control.Exception
import Network.Bugsnag.TH
import Network.Bugsnag.Types
import System.Environment
-- import Network.HTTP.Dispatch.Core
-- import Network.HTTP.Dispatch.Types
import Data.ByteString.Lazy as BL
import Data.Text
import qualified Data.Text as T
import Data.Aeson
import Language.Haskell.TH.Syntax
-- import           Network.HTTP.Client.TLS
import           Network.HTTP.Client

bugsnagNotifyUri :: String
bugsnagNotifyUri = "https://notify.bugsnag.com"

bugsnagHttpRequest :: BugsnagRequest -> IO Request
bugsnagHttpRequest bugsnagReq = do
  initReq <- parseRequest bugsnagNotifyUri
  let reqBody = BL.toStrict (encode bugsnagReq)
  return $ initReq { method = "POST"
                   , requestBody = RequestBodyBS reqBody }

-- [header "Content-Type" "application/json"]
-- dispatch appKey mgr = do
--   initReq <- liftIO $ parseUrl' bugSnagNotify
  -- let reqBody = RequestBodyLBS $ fromMaybe emptyBody body
  --   req <- liftIO $ reqHook $ setRequestIgnoreStatus $ initReq { method = dMethod
  --                                                              , requestBody = reqBody }
  --   liftIO $ httpLbs req mgr

-- (Just (BL.toStrict (encode $ testRequest key)))

-- dispatch :: MonadBH m
--          => Method
--          -> Text
--          -> Maybe L.ByteString
--          -> m Reply
-- dispatch dMethod url body = do
--   initReq <- liftIO $ parseUrl' url
--   reqHook <- bhRequestHook A.<$> getBHEnv
--   let reqBody = RequestBodyLBS $ fromMaybe emptyBody body
--   req <- liftIO $ reqHook $ setRequestIgnoreStatus $ initReq { method = dMethod
--                                                              , requestBody = reqBody }
--   mgr <- bhManager <$> getBHEnv
--   liftIO $ httpLbs req mgr
