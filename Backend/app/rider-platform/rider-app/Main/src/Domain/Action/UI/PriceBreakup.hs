{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Action.UI.PriceBreakup (getPriceBreakup) where

import qualified API.Types.UI.PriceBreakup
import Data.OpenApi (ToSchema)
import Domain.Action.UI.Quote
import qualified Domain.Action.UI.Quote as DQ
import qualified Domain.Types.Booking
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude hiding (id)
import Kernel.Beam.Functions as B
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Types.Price
import Kernel.Utils.Common
import Servant
import qualified Storage.Clickhouse.QuoteBreakup as CHQ
import qualified Storage.Queries.Booking as QRB
import Tools.Auth

getPriceBreakup ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.Booking.Booking ->
    Environment.Flow API.Types.UI.PriceBreakup.QuoteBreakupRes
  )
getPriceBreakup (_, _) bookingId = do
  booking <- B.runInReplica $ QRB.findById bookingId >>= fromMaybeM (BookingDoesNotExist bookingId.getId)
  quoteBreakups <- case booking.quoteId of
    Nothing -> pure []
    Just qId -> CHQ.findAllByQuoteId qId booking.createdAt
  pure $ API.Types.UI.PriceBreakup.QuoteBreakupRes (transformQuoteBreakup `map` quoteBreakups)

transformQuoteBreakup :: CHQ.QuoteBreakup -> DQ.QuoteBreakupAPIEntity
transformQuoteBreakup quoteBreakup =
  DQ.QuoteBreakupAPIEntity
    { title = CHQ.title quoteBreakup,
      priceWithCurrency = PriceAPIEntity (HighPrecMoney $ toRational (CHQ.priceValue quoteBreakup)) (CHQ.priceCurrency quoteBreakup)
    }
