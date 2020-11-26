{-# LANGUAGE TemplateHaskell #-}

module Beckn.Types.Core.Migration.Context (Context (..)) where

import Beckn.Types.Core.Migration.Domain (Domain)
import Beckn.Types.Core.Migration.Duration (Duration)
import Beckn.Utils.JSON (constructorsToLowerOptions, deriveJSON)
import Data.Time (UTCTime)
import EulerHS.Prelude
import Servant.Client (BaseUrl)

data Context = Context
  { _domain :: Domain,
    _country :: Text,
    _city :: Text,
    _action :: Action,
    _core_version :: Text,
    _bap_id :: BaseUrl,
    _bap_uri :: BaseUrl,
    _bpp_id :: Maybe BaseUrl,
    _bpp_uri :: Maybe BaseUrl,
    _transaction_id :: Text,
    _message_id :: Text,
    _timestamp :: UTCTime,
    _key :: Maybe Text,
    _ttl :: Maybe Duration
  }
  deriving (Generic, Show)

data Action
  = SEARCH
  | SELECT
  | INIT
  | CONFIRM
  | UPDATE
  | STATUS
  | TRACK
  | CANCEL
  | FEEDBACK
  | SUPPORT
  | ON_SEARCH
  | ON_SELECT
  | ON_INIT
  | ON_CONFIRM
  | ON_UPDATE
  | ON_STATUS
  | ON_TRACK
  | ON_CANCEL
  | ON_FEEDBACK
  | ON_SUPPORT
  | ACK
  deriving (Generic, Show)

deriveJSON ''Context 'stripLensPrefixOptions
deriveJSON ''Action 'constructorsToLowerOptions
