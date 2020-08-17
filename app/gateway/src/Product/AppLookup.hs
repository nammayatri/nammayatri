module Product.AppLookup
  ( insert,
    lookup,
  )
where

import App.Types
import Beckn.Storage.Redis.Queries
import EulerHS.Prelude

insert :: Text -> Text -> Flow ()
insert messageId appUrl =
  setExRedis messageId appUrl 1800 -- seconds

lookup :: Text -> Flow (Maybe Text)
lookup = getKeyRedis
