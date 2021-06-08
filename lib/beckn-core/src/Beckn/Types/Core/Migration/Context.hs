module Beckn.Types.Core.Migration.Context (Context (..)) where

import Beckn.Types.Core.Migration.Domain (Domain)
import Beckn.Types.Core.Migration.Duration (Duration)
import Beckn.Utils.JSON
import Data.Aeson
import Data.Time (UTCTime)
import EulerHS.Prelude
import Servant.Client (BaseUrl)

data Context = Context
  { domain :: Domain,
    country :: Text,
    city :: Text,
    action :: Text,
    core_version :: Text,
    bap_id :: BaseUrl,
    bap_uri :: BaseUrl,
    bpp_id :: Maybe BaseUrl,
    bpp_uri :: Maybe BaseUrl,
    transaction_id :: Text,
    message_id :: Text,
    timestamp :: UTCTime,
    key :: Maybe Text,
    ttl :: Maybe Duration
  }
  deriving (Generic, Show)

instance FromJSON Context where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Context where
  toJSON = genericToJSON $ stripPrefixUnderscoreIfAny {omitNothingFields = True}
