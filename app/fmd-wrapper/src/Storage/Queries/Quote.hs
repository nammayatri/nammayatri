module Storage.Queries.Quote where

import App.Types
import Beckn.Storage.Redis.Queries
import EulerHS.Prelude
import Types.Wrapper (OrderDetails)

-- To be deleted after migration tested

lookupQuote :: Text -> Flow (Maybe OrderDetails)
lookupQuote key =
  getKeyRedis ("Quote_" <> key)

storeQuote :: Text -> OrderDetails -> Flow ()
storeQuote quoteId quote = do
  let ttl = 86400
  setExRedis ("Quote_" <> quoteId) quote ttl
  return ()
