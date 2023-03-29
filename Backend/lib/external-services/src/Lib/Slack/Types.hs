{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Lib.Slack.Types where

import Data.Aeson.Types
import Data.OpenApi
import EulerHS.Prelude hiding (state)
import Kernel.Types.Error.BaseError.HTTPError hiding (Error)
import Kernel.Types.Error.BaseError.HTTPError.FromResponse
import Kernel.Utils.Dhall (FromDhall)
import Kernel.Utils.JSON

data SlackConfig = SlackConfig
  { slackToken :: Text,
    channelName :: Text
  }
  deriving (Generic, Eq, Show, FromJSON, ToJSON, FromDhall)

data SlackRequest = SlackRequest
  { channel :: Text,
    blocks :: Maybe [Block (Block Text)]
  }
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

data Block a = Block
  { _type :: Text,
    _text :: a
  }
  deriving (Generic, Eq, Show)

instance (FromJSON a) => FromJSON (Block a) where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance (ToJSON a) => ToJSON (Block a) where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

data SlackResponse = SlackResponse
  {ok :: Bool, _error :: Maybe Text}
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

newtype Error = Error {message :: Text}
  deriving (Show, Generic)

instance IsAPIError Error

deriving newtype instance ToJSON Error

deriving newtype instance FromJSON Error

deriving newtype instance ToSchema Error

instance FromResponse Error where
  fromResponse = fromJsonResponse

instance IsBaseError Error where
  toMessage _ = Just "SLACK_ERROR"

instance IsHTTPError Error where
  toErrorCode _ = "CORE002"
  toHttpCode _ = E404

instance IsBecknAPIError Error where
  toType _ = DOMAIN_ERROR -- only to satisfy current tests, FIXME maybe

instanceExceptionWithParent 'HTTPException ''Error
