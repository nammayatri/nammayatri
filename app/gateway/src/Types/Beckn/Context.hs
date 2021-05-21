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
  { _domain :: Domain, -- for now using domain from 0.8
    _country :: Maybe Text, -- Maybe in 0.8 only
    _city :: Maybe Text, -- Maybe in 0.8 only
    _action :: Text, -- becomes sumtype in 0.9. for now using Text like in 0.8
    _core_version :: Maybe Text, -- Maybe in 0.8 only
    _domain_version :: Maybe Text, -- present only in 0.8
    _bap_id :: Maybe BaseUrl, -- present only in 0.9
    _bap_uri :: Maybe BaseUrl, -- Maybe in 0.8 only
    _bpp_id :: Maybe BaseUrl, -- present only in 0.9
    _bpp_uri :: Maybe BaseUrl,
    _transaction_id :: Text,
    _message_id :: Text,
    _timestamp :: UTCTime,
    _key :: Maybe Text, -- present only in 0.9
    _ttl :: Maybe Text -- becomes newtype in 0.9. for now using Text like in 0.8
  }
  deriving (Generic, Show)

instance FromJSON Context where
  parseJSON = genericParseJSON stripLensPrefixOptions

instance ToJSON Context where
  toJSON = genericToJSON $ stripAllLensPrefixOptions {omitNothingFields = True}
