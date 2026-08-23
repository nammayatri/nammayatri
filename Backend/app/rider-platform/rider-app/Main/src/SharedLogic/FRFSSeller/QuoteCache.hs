module SharedLogic.FRFSSeller.QuoteCache
  ( SellerQuote (..),
    cacheQuotes,
    findQuote,
    holdQuote,
  )
where

import qualified Data.Text as T
import Environment (Flow)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Utils.Common

data SellerQuote = SellerQuote
  { itemId :: Text,
    journeyId :: Text,
    providerId :: Text,
    providerName :: Text,
    fromStopCode :: Text,
    toStopCode :: Text,
    fromStopName :: Text,
    toStopName :: Text,
    priceValue :: Text,
    currency :: Text,
    maxTicketsPerOrder :: Int,
    fareQuoteId :: Maybe Text
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON)

quoteKey :: Text -> Text -> Text -> Text
quoteKey operator transactionId itemId = "frfsSeller:quote:" <> operator <> ":" <> transactionId <> ":" <> itemId

cacheQuotes :: Redis.ExpirationTime -> Text -> Text -> [SellerQuote] -> Flow ()
cacheQuotes ttlSeconds operator transactionId quotes =
  forM_ quotes $ \quote ->
    try @_ @SomeException
      (Redis.withCrossAppRedis $ Redis.setExp (quoteKey operator transactionId quote.itemId) quote ttlSeconds)
      >>= \case
        Right () -> pure ()
        Left err ->
          logWarning $ "FRFS seller quote cache write failed for " <> quote.itemId <> ": " <> T.pack (show err)

findQuote :: Text -> Text -> Text -> Flow (Maybe SellerQuote)
findQuote operator transactionId itemId =
  Redis.withCrossAppRedis $ Redis.safeGet (quoteKey operator transactionId itemId)

holdQuote :: Redis.ExpirationTime -> Text -> Text -> SellerQuote -> Flow ()
holdQuote heldTtlSeconds operator transactionId quote =
  try @_ @SomeException
    (Redis.withCrossAppRedis $ Redis.setExp (quoteKey operator transactionId quote.itemId) quote heldTtlSeconds)
    >>= \case
      Right () -> pure ()
      Left err ->
        logWarning $ "FRFS seller quote hold failed for " <> quote.itemId <> ": " <> T.pack (show err)
