module Domain.Action.UI.PriceBreakup (getPriceBreakup) where

import qualified API.Types.UI.PriceBreakup
import qualified Data.Aeson as A
import qualified Data.Text.Encoding as TE
import Domain.Action.UI.Quote
import qualified Domain.Action.UI.Quote as DQ
import qualified Domain.Types.Booking
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Domain.Types.Quote as DQuote
import qualified Environment
import EulerHS.Prelude hiding (id)
import Kernel.Beam.Functions as B
import qualified Kernel.Prelude
import Kernel.Storage.ClickhouseV2 as CH
import qualified Kernel.Types.Common as KTC
import Kernel.Types.Error
import qualified Kernel.Types.Id
import qualified Kernel.Types.Id as Id
import Kernel.Types.Price
import Kernel.Utils.Common
import qualified Storage.Clickhouse.Quote as CHQuote
import qualified Storage.Clickhouse.QuoteBreakup as CHQ
import qualified Storage.Queries.QueriesExtra.BookingLite as QBookingLite

getPriceBreakup ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.Booking.Booking ->
    Environment.Flow API.Types.UI.PriceBreakup.QuoteBreakupRes
  )
getPriceBreakup (_, _) bookingId = do
  booking <- B.runInReplica $ QBookingLite.findByIdLite bookingId >>= fromMaybeM (BookingDoesNotExist bookingId.getId)
  entities <- case booking.quoteId of
    Nothing -> pure []
    Just qId -> findQuoteBreakupCH qId booking.createdAt
  pure $ API.Types.UI.PriceBreakup.QuoteBreakupRes entities

-- | Slim JSON shape mirroring
-- 'Storage.Queries.Transformers.Quote.QuoteBreakupItem'. Duplicated here to
-- keep the dependency direction clean (UI handler doesn't import Storage
-- transformer internals).
data QuoteBreakupItemJson = QuoteBreakupItemJson
  { title :: Text,
    price :: KTC.Price
  }
  deriving (Generic)

instance A.FromJSON QuoteBreakupItemJson

-- | Try the new quote.breakup_list_json Clickhouse column first; fall back
-- to the legacy quote_breakup Clickhouse table on cache miss or parse
-- failure. Lets us migrate without a flag-day.
findQuoteBreakupCH ::
  (MonadFlow m, CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m) =>
  Id.Id DQuote.Quote ->
  UTCTime ->
  m [DQ.QuoteBreakupAPIEntity]
findQuoteBreakupCH quoteId createdAt = do
  mQuote <- CHQuote.findById quoteId createdAt
  let mItems = do
        q <- mQuote
        jsonText <- q.breakupListJson
        A.decodeStrict (TE.encodeUtf8 jsonText) :: Maybe [QuoteBreakupItemJson]
  case mItems of
    Just items@(_ : _) -> pure $ map itemToApi items
    _ -> do
      legacy <- CHQ.findAllByQuoteId quoteId createdAt
      pure $ transformQuoteBreakup <$> legacy
  where
    itemToApi item =
      DQ.QuoteBreakupAPIEntity
        { title = item.title,
          priceWithCurrency = PriceAPIEntity item.price.amount item.price.currency
        }

transformQuoteBreakup :: CHQ.QuoteBreakup -> DQ.QuoteBreakupAPIEntity
transformQuoteBreakup quoteBreakup =
  DQ.QuoteBreakupAPIEntity
    { title = CHQ.title quoteBreakup,
      priceWithCurrency = PriceAPIEntity (HighPrecMoney $ toRational (CHQ.priceValue quoteBreakup)) (CHQ.priceCurrency quoteBreakup)
    }
