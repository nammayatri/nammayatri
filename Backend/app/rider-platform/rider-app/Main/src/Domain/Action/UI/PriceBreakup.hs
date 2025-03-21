module Domain.Action.UI.PriceBreakup (getPriceBreakup, getMeterRidePrice) where

import qualified API.Types.UI.PriceBreakup
import Domain.Action.UI.Quote
import qualified Domain.Action.UI.Quote as DQ
import qualified Domain.Types.Booking
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Domain.Types.Ride
import qualified Environment
import EulerHS.Prelude hiding (id)
import Kernel.Beam.Functions as B
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Types.Price
import Kernel.Utils.Common
import SharedLogic.CallBPPInternal as CallBPPInternal
import qualified Storage.CachedQueries.Merchant as QMerchant
import qualified Storage.Clickhouse.QuoteBreakup as CHQ
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.MerchantOperatingCity as QMerchantO
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Ride as QRide

getMeterRidePrice ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.Ride.Ride ->
    Environment.Flow API.Types.UI.PriceBreakup.MeterRidePriceRes
  )
getMeterRidePrice (Nothing, _) rideId = throwError . InternalError $ "PersonId is requried while fetching meter ride fare: " <> rideId.getId
getMeterRidePrice (Just personId, _) rideId = do
  person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  ride <- B.runInReplica $ QRide.findById rideId >>= fromMaybeM (RideNotFound rideId.getId)
  merchant <- QMerchant.findById person.merchantId >>= fromMaybeM (MerchantNotFound person.merchantId.getId)
  merchantOpCity <- QMerchantO.findById person.merchantOperatingCityId >>= fromMaybeM (MerchantOperatingCityNotFound person.merchantOperatingCityId.getId)
  mOpCityId <- merchantOpCity.driverOfferMerchantOperatingCityId & fromMaybeM (MerchantOperatingCityNotFound $ "Need driver offer merchant operating city id to be congifured in rider side merchant operating city table.")
  res <- CallBPPInternal.meterRideFare merchant.driverOfferApiKey merchant.driverOfferBaseUrl merchant.driverOfferMerchantId mOpCityId (ride.bppRideId.getId)
  pure $
    API.Types.UI.PriceBreakup.MeterRidePriceRes
      { distance = res.distance,
        fare = res.fare
      }

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
