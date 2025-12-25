module Domain.Action.UI.EstimateBP where

import qualified "dashboard-helper-api" API.Types.RiderPlatform.Management.Ride as Common
import API.Types.UI.EstimateBP as DTEst
import qualified API.Types.UI.EstimateBP
import qualified Domain.Types.InterCityDetails
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.Person as Person
import qualified Domain.Types.Quote as DQuote
import qualified Domain.Types.RentalDetails
import qualified Domain.Types.Ride as Ride
import qualified Environment
import EulerHS.Prelude hiding (id)
import Kernel.Beam.Functions as B
import qualified Kernel.Prelude
import Kernel.Storage.ClickhouseV2 as CH
import Kernel.Types.Error
import qualified Kernel.Types.Id as Id
import Kernel.Types.Price (PriceAPIEntity (..))
import Kernel.Utils.Common
import qualified Storage.Clickhouse.EstimateBreakup as CH
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.Quote as QQuote
import qualified Storage.Queries.Ride as QRide

getRideEstimateBreakup :: ((Kernel.Prelude.Maybe (Id.Id Person.Person), Id.Id Merchant.Merchant) -> Id.Id Ride.Ride -> Environment.Flow API.Types.UI.EstimateBP.EstimateDetailsRes)
getRideEstimateBreakup (_, _) rideId_ = do
  ride <- B.runInReplica $ QRide.findById rideId_ >>= fromMaybeM (RideDoesNotExist rideId_.getId)
  booking <- B.runInReplica $ QRB.findById ride.bookingId >>= fromMaybeM (BookingDoesNotExist ride.bookingId.getId)
  case booking.quoteId of
    Just quoteId -> do
      quote <- B.runInReplica $ QQuote.findById quoteId >>= fromMaybeM (QuoteDoesNotExist quoteId.getId)
      estimateBreakup <- getEstimateBreakupFromQuote quote
      pure $ API.Types.UI.EstimateBP.EstimateDetailsRes {estimateBreakup = estimateBreakup}
    Nothing -> pure $ API.Types.UI.EstimateBP.EstimateDetailsRes {estimateBreakup = []}

getEstimateBreakupFromQuote :: (EsqDBFlow m r, MonadFlow m, CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m) => DQuote.Quote -> m [API.Types.UI.EstimateBP.EstimateBreakup]
getEstimateBreakupFromQuote quote =
  case quote.quoteDetails of
    DQuote.OneWayDetails _ -> pure []
    DQuote.AmbulanceDetails _ -> pure []
    DQuote.MeterRideDetails _ -> pure []
    DQuote.DeliveryDetails driverOfferDetails -> do
      breakup <- CH.findAllByEstimateIdT (Id.cast driverOfferDetails.estimateId) quote.createdAt
      pure $ transformEstimate <$> breakup
    DQuote.InterCityDetails interCityDetails -> pure $ transformInterCityDetails interCityDetails
    DQuote.RentalDetails rentalDetails -> pure $ transformRentalDetails rentalDetails
    DQuote.DriverOfferDetails driverOfferDetails -> do
      breakup <- CH.findAllByEstimateIdT (Id.cast driverOfferDetails.estimateId) quote.createdAt
      pure $ transformEstimate <$> breakup
    DQuote.OneWaySpecialZoneDetails _ -> pure []

transformEstimate :: CH.EstimateBreakup -> API.Types.UI.EstimateBP.EstimateBreakup
transformEstimate estimate =
  API.Types.UI.EstimateBP.EstimateBreakup
    { price =
        API.Types.UI.EstimateBP.EstimateBreakupPrice
          { value =
              Kernel.Types.Price.PriceAPIEntity
                { amount = CH.priceValue estimate,
                  currency = CH.priceCurrency estimate
                }
          },
      title = CH.title estimate
    }

transformEstimate' :: API.Types.UI.EstimateBP.EstimateBreakup -> Common.EstimateBreakup
transformEstimate' API.Types.UI.EstimateBP.EstimateBreakup {..} = do
  Common.EstimateBreakup
    { price =
        Common.EstimateBreakupPrice
          { value =
              Kernel.Types.Price.PriceAPIEntity
                { amount = price.value.amount,
                  currency = price.value.currency
                }
          },
      title = title
    }

createEstimateBreakup :: Price -> Text -> API.Types.UI.EstimateBP.EstimateBreakup
createEstimateBreakup price title =
  API.Types.UI.EstimateBP.EstimateBreakup
    { price =
        API.Types.UI.EstimateBP.EstimateBreakupPrice
          { value =
              Kernel.Types.Price.PriceAPIEntity
                { amount = price.amount,
                  currency = price.currency
                }
          },
      title = title
    }

transformInterCityDetails :: Domain.Types.InterCityDetails.InterCityDetails -> [API.Types.UI.EstimateBP.EstimateBreakup]
transformInterCityDetails interCityDetails =
  [ createEstimateBreakup interCityDetails.baseFare "Base Fare",
    createEstimateBreakup interCityDetails.deadKmFare "Dead Km Fare",
    createEstimateBreakup interCityDetails.perExtraKmRate "Per Extra Km Rate",
    createEstimateBreakup interCityDetails.perExtraMinRate "Per Extra Min Rate",
    createEstimateBreakup interCityDetails.perHourCharge "Per Hour Charge",
    createEstimateBreakup interCityDetails.plannedPerKmRateOneWay "Planned Per Km Rate One Way",
    createEstimateBreakup interCityDetails.plannedPerKmRateRoundTrip "Planned Per Km Rate Round Trip"
  ]

transformRentalDetails :: Domain.Types.RentalDetails.RentalDetails -> [API.Types.UI.EstimateBP.EstimateBreakup]
transformRentalDetails rentalDetails =
  [ createEstimateBreakup rentalDetails.baseFare "Base Fare",
    createEstimateBreakup rentalDetails.deadKmFare "Dead Km Fare",
    createEstimateBreakup rentalDetails.perExtraKmRate "Per Extra Km Rate",
    createEstimateBreakup rentalDetails.perExtraMinRate "Per Extra Min Rate",
    createEstimateBreakup rentalDetails.perHourCharge "Per Hour Charge",
    createEstimateBreakup rentalDetails.plannedPerKmRate "Planned Per Km Rate"
  ]
