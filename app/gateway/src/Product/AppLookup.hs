module Product.AppLookup where

import App.Types
import Beckn.Storage.Redis.Queries
import Beckn.Types.App
import Beckn.Types.Core.Context
import EulerHS.Prelude

data GwSession = GwSession
  { cbUrl :: BaseUrl,
    cbApiKey :: Text,
    searchContext :: Context
  }
  deriving (Generic, Show, ToJSON, FromJSON)

cacheNamespace :: Text
cacheNamespace = "beckn_gateway:"

insert :: Text -> GwSession -> Flow ()
insert messageId appUrl =
  setExRedis (cacheNamespace <> messageId) appUrl 1800 -- seconds

lookup :: Text -> Flow (Maybe GwSession)
lookup messageId = getKeyRedis (cacheNamespace <> messageId)