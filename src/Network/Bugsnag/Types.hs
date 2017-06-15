{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Network.Bugsnag.Types where

import           Data.Aeson
import           Data.List.NonEmpty
import           Data.Text
import           GHC.Generics

data BugsnagEvent = BugsnagEvent
  { bugsnagExceptions :: ![BugsnagException]
  , bugsnagThreads :: !(Maybe [BugsnagThread])
  , bugsnagContext :: !(Maybe Text)
  , bugsnagGroupingHash :: !(Maybe Text)
  , bugsnagSeverity :: !(Maybe Text)
  , bugsnagUser :: !BugsnagUser
  , bugsnagApp :: !BugsnagApp
  , bugsnagDevice :: !BugsnagDevice
  , bugsnagMetaData :: !(Maybe Value)
  } deriving (Show, Eq)

instance ToJSON BugsnagEvent where
  toJSON BugsnagEvent{..} = object
    [ "payloadVersion" .= String "2"
    , "exceptions" .= bugsnagExceptions
    , "threads" .= bugsnagThreads
    , "context" .= bugsnagContext
    , "groupingHash" .= bugsnagGroupingHash
    , "severity" .= bugsnagSeverity
    , "user" .= bugsnagUser
    , "app" .= bugsnagApp
    , "device" .= bugsnagDevice
    , "metaData" .= bugsnagMetaData
    ]

data BugsnagThread = BugsnagThread
  { bugsnagThreadId :: !Text
  , bugsnagThreadName :: !Text
    -- Stackframes cannot be an empty list or Bugsnag
    -- will throw an error
  , bugsnagThreadStackTrace :: !(NonEmpty BugsnagStackFrame)
  } deriving (Show, Eq)

instance ToJSON BugsnagThread where
  toJSON BugsnagThread{..} = object
    [ "id" .= bugsnagThreadId
    , "name" .= bugsnagThreadName
    , "stacktrace" .= bugsnagThreadStackTrace
    ]

data BugsnagStackFrame = BugsnagStackFrame
  { stackFrameFile :: !Text
  , stackFrameMethod :: !Text
  , stackFrameLineNumber :: !Int
  , stackFrameColumnNumber :: !(Maybe Int)
  , stackFrameInProject :: !Bool
  , stackFrameCode :: !Object
  } deriving (Show, Eq)

instance ToJSON BugsnagStackFrame where
  toJSON BugsnagStackFrame{..} = object
    [ "file" .= stackFrameFile
    , "method" .= stackFrameMethod
    , "lineNumber" .= stackFrameLineNumber
    , "columnNumber" .= stackFrameColumnNumber
    , "inProject" .= stackFrameInProject
    , "code" .= stackFrameCode
    ]

data BugsnagException = BugsnagException
  { exceptionErrorClass :: !Text
  , exceptionMessage :: !Text
    -- Bugsnag throws an error if this is empty
  , exceptionStacktrace :: !(NonEmpty BugsnagStackFrame)
  } deriving (Show, Eq)

instance ToJSON BugsnagException where
  toJSON BugsnagException{..} = object
    [ "errorClass" .= exceptionErrorClass
    , "message" .= exceptionMessage
    , "stacktrace" .= exceptionStacktrace
    ]


data BugsnagUser = BugsnagUser
  { bugsnagUserId :: !(Maybe Text)
  , bugsnagUserName :: !(Maybe Text)
  , bugsnagUserEmail :: !(Maybe Text)
  } deriving (Show, Eq)

instance ToJSON BugsnagUser where
  toJSON BugsnagUser{..} = object
    [ "id" .= bugsnagUserId
    , "name" .= bugsnagUserName
    , "email" .= bugsnagUserEmail
    ]

data BugsnagApp  = BugsnagApp
  { bugsnagAppVersion :: !(Maybe Text)
  , bugsnagReleaseStage :: !(Maybe Text)
  , bugsnagAppType :: !(Maybe Text)
  } deriving (Show, Eq)

instance ToJSON BugsnagApp where
  toJSON BugsnagApp{..} = object
    [ "version" .= bugsnagAppVersion
    , "releaseStage" .= bugsnagReleaseStage
    , "type" .= bugsnagAppType
    ]

data BugsnagDevice = BugsnagDevice
  { bugsnagOsVersion :: !(Maybe Text)
  , bugsnagHostname :: !(Maybe Text)
  } deriving (Show, Eq)

instance ToJSON BugsnagDevice where
  toJSON BugsnagDevice{..} = object
    [ "osVersion" .= bugsnagOsVersion
    , "hostname" .= bugsnagHostname
    ]

newtype BugsnagApiKey =
  BugsnagApiKey { unBugsnagApiKey :: Text }
  deriving (Show, Eq, Generic)

instance ToJSON BugsnagApiKey

data BugsnagRequest = BugsnagRequest
  { bugsnagApiKey :: !BugsnagApiKey
  , bugsnagEvents :: ![BugsnagEvent]
  } deriving (Show, Eq)

instance ToJSON BugsnagRequest where
  toJSON BugsnagRequest{..} =
    object
      [ "apiKey" .= unBugsnagApiKey bugsnagApiKey
      , "notifier" .= object
        [ "name" .= String "Haskell Notifier"
        , "version" .= String "1.0.1.1"
        , "url" .= String "https://github.com/5outh/bugsnag-haskell"
        ]
      , "events" .= toJSON bugsnagEvents
      ]

class BugsnagError e where
  errorRow :: e -> Int
  errorColumn :: e -> Maybe Int
  errorName :: e -> Text
  errorMessage ::  e -> Maybe Text
  errorFile :: e -> Maybe Text
