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

insert :: Text -> GwSession -> Flow ()
insert messageId appUrl =
  setExRedis messageId appUrl 1800 -- seconds

lookup :: Text -> Flow (Maybe GwSession)
lookup = getKeyRedis

incrSearchReqCount :: Text -> Flow (Maybe Integer)
incrSearchReqCount messageId =
  incrementKeyRedis (messageId <> "_search_count")

incrSearchErrCount :: Text -> Flow (Maybe Integer)
incrSearchErrCount messageId =
  incrementKeyRedis (messageId <> "_search_error")

incrOnSearchReqCount :: Text -> Flow (Maybe Integer)
incrOnSearchReqCount messageId =
  incrementKeyRedis (messageId <> "_onsearch_count")

getRequestStatus :: Text -> Flow (Integer, Integer, Integer)
getRequestStatus messageId = do
  searchReqCount <- getKeyRedis (messageId <> "_search_count")
  searchErrCount <- getKeyRedis (messageId <> "_search_error")
  onSearchReqCount <- getKeyRedis (messageId <> "_onsearch_count")
  return (fromMaybe 0 searchReqCount, fromMaybe 0 searchErrCount, fromMaybe 0 onSearchReqCount)

cleanup :: Text -> Flow ()
cleanup messageId =
  void $
    deleteKeysRedis
      [ messageId <> "_search_count",
        messageId <> "_search_error",
        messageId <> "_onsearch_count"
      ]
