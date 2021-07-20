module Beckn.Types.Core.Migration.Context where

import Beckn.Types.Core.Migration.Domain (Domain)
import Beckn.Types.Time (Iso8601Time)
import Beckn.Utils.JSON
import Data.Aeson
import Data.Time (UTCTime)
import EulerHS.Prelude
import Servant.Client (BaseUrl)

data Context = Context
  { domain :: Domain,
    country :: Text,
    city :: Text,
    action :: Action,
    core_version :: Text,
    bap_id :: BaseUrl,
    bap_uri :: BaseUrl,
    bpp_id :: Maybe Text,
    bpp_uri :: Maybe BaseUrl,
    transaction_id :: Text,
    message_id :: Text,
    timestamp :: UTCTime,
    key :: Maybe Text,
    ttl :: Maybe Iso8601Time
  }
  deriving (Generic, Show)

instance FromJSON Context where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Context where
  toJSON = genericToJSON $ stripPrefixUnderscoreIfAny {omitNothingFields = True}

data Action
  = SEARCH
  | SELECT
  | INIT
  | CONFIRM
  | UPDATE
  | STATUS
  | TRACK
  | CANCEL
  | RATING
  | SUPPORT
  | ON_SEARCH
  | ON_SELECT
  | ON_INIT
  | ON_CONFIRM
  | ON_UPDATE
  | ON_STATUS
  | ON_TRACK
  | ON_CANCEL
  | ON_RATING
  | ON_SUPPORT
  deriving (Generic, Show, Eq)

instance FromJSON Action where
  parseJSON = genericParseJSON constructorsToLowerOptions

instance ToJSON Action where
  toJSON = genericToJSON constructorsToLowerOptions

mapToCbAction :: Action -> Maybe Action
mapToCbAction = \case
  SEARCH -> Just ON_SEARCH
  SELECT -> Just ON_SELECT
  INIT -> Just ON_INIT
  CONFIRM -> Just ON_CONFIRM
  UPDATE -> Just ON_UPDATE
  STATUS -> Just ON_STATUS
  TRACK -> Just ON_TRACK
  CANCEL -> Just ON_CANCEL
  RATING -> Just ON_RATING
  SUPPORT -> Just ON_SUPPORT
  _ -> Nothing
