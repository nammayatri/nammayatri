module Beckn.Types.Core.Migration1.Common.Context
  ( module Beckn.Types.Core.Migration1.Common.Context,
    module Reexport,
  )
where

import Beckn.Types.App
import Beckn.Types.Core.Migration1.Common.Context.Domain as Reexport
import Beckn.Utils.Example
import Data.Aeson
import Data.OpenApi (ToSchema)
import Data.Time (UTCTime)
import EulerHS.Prelude
import Servant.Client (parseBaseUrl)

data Context = Context
  { domain :: Domain,
    core_version :: Text,
    bap_id :: Maybe Text,
    bap_uri :: BaseUrl,
    bpp_id :: Maybe Text,
    bpp_uri :: Maybe BaseUrl,
    transaction_id :: Text,
    message_id :: Text,
    timestamp :: UTCTime
  }
  deriving (Generic, FromJSON, Show, ToSchema)

instance ToJSON Context where
  toJSON = genericToJSON $ defaultOptions {omitNothingFields = True}

instance Example Context where
  example =
    Context
      { domain = example,
        core_version = "0.9.3",
        bap_id = Just "",
        bap_uri = fromJust $ parseBaseUrl "https://api.domain.com/",
        bpp_id = Just "",
        bpp_uri = parseBaseUrl "https://api.domain.com/",
        transaction_id = idExample,
        message_id = idExample,
        timestamp = example
      }
