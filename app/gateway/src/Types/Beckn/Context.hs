module Types.Beckn.Context where

import Data.Aeson
import Data.Text
import EulerHS.Prelude hiding ((.=))
import Servant.Client (BaseUrl)
import Types.Beckn.Domain

data Context = Context
  { domain :: Domain,
    bap_uri :: BaseUrl,
    bpp_uri :: Maybe BaseUrl,
    transaction_id :: Text
  }
  deriving (Generic, FromJSON, Show)

instance ToJSON Context where
  toJSON = genericToJSON $ defaultOptions {omitNothingFields = True}
