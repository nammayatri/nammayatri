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

incrSearchReqCount :: Text -> Flow (Maybe Integer)
incrSearchReqCount messageId =
  incrementKeyRedis (cacheNamespace <> messageId <> "_search_count")

incrSearchErrCount :: Text -> Flow (Maybe Integer)
incrSearchErrCount messageId =
  incrementKeyRedis (cacheNamespace <> messageId <> "_search_error")

incrOnSearchReqCount :: Text -> Flow (Maybe Integer)
incrOnSearchReqCount messageId =
  incrementKeyRedis (cacheNamespace <> messageId <> "_onsearch_count")

getRequestStatus :: Text -> Flow (Integer, Integer, Integer)
getRequestStatus messageId = do
  searchReqCount <- getKeyRedis (cacheNamespace <> messageId <> "_search_count")
  searchErrCount <- getKeyRedis (cacheNamespace <> messageId <> "_search_error")
  onSearchReqCount <- getKeyRedis (cacheNamespace <> messageId <> "_onsearch_count")
  return (fromMaybe 0 searchReqCount, fromMaybe 0 searchErrCount, fromMaybe 0 onSearchReqCount)

cleanup :: Text -> Flow ()
cleanup messageId =
  void $
    deleteKeysRedis
      [ cacheNamespace <> messageId <> "_search_count",
        cacheNamespace <> messageId <> "_search_error",
        cacheNamespace <> messageId <> "_onsearch_count"
      ]
