module Types.Beckn.Context where

import Data.Aeson
import Data.Text
import Data.Time
import EulerHS.Prelude hiding ((.=))
import Servant.Client (BaseUrl)
import Types.Beckn.Domain

-- Specific Context for working with both 0.8 and 0.9 spec versions
-- TODO: use 0.9 Context once all domains will migrate to 0.9
data Context = Context
  { domain :: Domain, -- for now using domain from 0.8
    country :: Maybe Text, -- Maybe in 0.8 only
    city :: Maybe Text, -- Maybe in 0.8 only
    action :: Text, -- becomes sumtype in 0.9. for now using Text like in 0.8
    core_version :: Maybe Text, -- Maybe in 0.8 only
    domain_version :: Maybe Text, -- present only in 0.8
    bap_id :: Maybe Text, -- present only in 0.9
    bap_uri :: Maybe BaseUrl, -- Maybe in 0.8 only
    bpp_id :: Maybe Text, -- present only in 0.9
    bpp_uri :: Maybe BaseUrl,
    transaction_id :: Text,
    message_id :: Text,
    timestamp :: UTCTime,
    key :: Maybe Text, -- present only in 0.9
    ttl :: Maybe Text -- becomes newtype in 0.9. for now using Text like in 0.8
  }
  deriving (Generic, FromJSON, Show)

instance ToJSON Context where
  toJSON = genericToJSON $ defaultOptions {omitNothingFields = True}
