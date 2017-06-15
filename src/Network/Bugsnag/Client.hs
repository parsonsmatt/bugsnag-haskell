module Network.Bugsnag.Client where

import           Control.Exception
import           Network.Bugsnag.TH
import           Network.Bugsnag.Types
import           System.Environment
-- import Network.HTTP.Dispatch.Core
-- import Network.HTTP.Dispatch.Types
import           Data.Aeson
import           Data.ByteString.Lazy       as BL
import           Data.Text
import qualified Data.Text                  as T
import           Language.Haskell.TH.Syntax
-- import           Network.HTTP.Client.TLS
import           Network.HTTP.Client

bugsnagNotifyUri :: String
bugsnagNotifyUri = "https://notify.bugsnag.com"

bugsnagHttpRequest :: BugsnagRequest -> IO Request
bugsnagHttpRequest bugsnagReq = do
  initReq <- parseRequest bugsnagNotifyUri
  let reqBody = BL.toStrict (encode bugsnagReq)
  return $ initReq { method = "POST"
                   , requestBody = RequestBodyBS reqBody
                   , requestHeaders = [("Content-Type", "application/json")]
                   }
