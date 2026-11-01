module Domain.Action.UI.EstimateBP where

import qualified "this" API.Types.RiderPlatform.Management.Ride as Common
import API.Types.UI.EstimateBP as DTEst
import qualified API.Types.UI.EstimateBP
import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Domain.Types.Booking as DBooking
import qualified Domain.Types.Estimate as DE
import qualified Domain.Types.InterCityDetails
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.Person as Person
import qualified Domain.Types.Quote as DQuote
import qualified Domain.Types.QuoteBreakup as DQB
import qualified Domain.Types.RentalDetails
import qualified Domain.Types.Ride as Ride
import qualified Environment
import EulerHS.Prelude hiding (id)
import Kernel.Beam.Functions as B
import qualified Kernel.Prelude
import Kernel.Storage.ClickhouseV2 as CH
import qualified Kernel.Types.Common as KTC
import qualified Kernel.Types.Distance as KDistance
import Kernel.Types.Error
import qualified Kernel.Types.Id as Id
import Kernel.Types.Price (PriceAPIEntity (..))
import Kernel.Utils.Common
import qualified Storage.Clickhouse.Estimate as CHE
import qualified Storage.Clickhouse.EstimateBreakup as CH
import qualified Storage.Queries.QueriesExtra.BookingLite as QBookingLite
import qualified Storage.Queries.QueriesExtra.RideLite as QRideLite
import qualified Storage.Queries.Quote as QQuote

getRideEstimateBreakup :: ((Kernel.Prelude.Maybe (Id.Id Person.Person), Id.Id Merchant.Merchant) -> Id.Id Ride.Ride -> Environment.Flow API.Types.UI.EstimateBP.EstimateDetailsRes)
getRideEstimateBreakup (_, _) rideId_ = do
  ride <- B.runInReplica $ QRideLite.findByIdLite rideId_ >>= fromMaybeM (RideDoesNotExist rideId_.getId)
  booking <- B.runInReplica $ QBookingLite.findByIdLite ride.bookingId >>= fromMaybeM (BookingDoesNotExist ride.bookingId.getId)
  getBookingEstimateBreakup booking

getRideBookingEstimateBreakup :: ((Kernel.Prelude.Maybe (Id.Id Person.Person), Id.Id Merchant.Merchant) -> Id.Id DBooking.Booking -> Environment.Flow API.Types.UI.EstimateBP.EstimateDetailsRes)
getRideBookingEstimateBreakup (_, _) bookingId = do
  booking <- B.runInReplica $ QBookingLite.findByIdLite bookingId >>= fromMaybeM (BookingDoesNotExist bookingId.getId)
  getBookingEstimateBreakup booking

getBookingEstimateBreakup :: QBookingLite.BookingLite -> Environment.Flow API.Types.UI.EstimateBP.EstimateDetailsRes
getBookingEstimateBreakup booking =
  case booking.quoteId of
    Just quoteId -> do
      quote <- B.runInReplica $ QQuote.findById quoteId >>= fromMaybeM (QuoteDoesNotExist quoteId.getId)
      estimateBreakup <- getEstimateBreakupFromQuote booking.estimatedDistance booking.estimatedDuration quote
      pure $ API.Types.UI.EstimateBP.EstimateDetailsRes {estimateBreakup = estimateBreakup}
    Nothing -> pure $ API.Types.UI.EstimateBP.EstimateDetailsRes {estimateBreakup = []}

getEstimateBreakupFromQuote :: (EsqDBFlow m r, MonadFlow m, CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m) => Kernel.Prelude.Maybe KTC.Distance -> Kernel.Prelude.Maybe KTC.Seconds -> DQuote.Quote -> m [API.Types.UI.EstimateBP.EstimateBreakup]
getEstimateBreakupFromQuote mbEstimatedDistance mbEstimatedDuration quote =
  case quote.quoteDetails of
    DQuote.OneWayDetails _ -> pure oneWayRateCard
    DQuote.AmbulanceDetails _ -> pure []
    DQuote.MeterRideDetails _ -> pure []
    DQuote.DeliveryDetails driverOfferDetails ->
      findEstimateBreakupCH (Id.cast driverOfferDetails.estimateId) quote.createdAt
    DQuote.InterCityDetails interCityDetails -> pure $ transformInterCityDetails interCityDetails
    DQuote.RentalDetails rentalDetails -> pure $ transformRentalDetails rentalDetails
    DQuote.DriverOfferDetails driverOfferDetails ->
      findEstimateBreakupCH (Id.cast driverOfferDetails.estimateId) quote.createdAt
    DQuote.OneWaySpecialZoneDetails _ -> pure oneWayRateCard
    -- EasyBooking's quote details reuse RentalDetails's shape, so the same transformer applies.
    DQuote.EasyBookingDetails rentalDetails -> pure $ transformRentalDetails rentalDetails
  where
    oneWayRateCard = transformOneWayRateCard (KDistance.distanceToMeters <$> mbEstimatedDistance) mbEstimatedDuration quote.quoteBreakupList

transformOneWayRateCard :: Kernel.Prelude.Maybe KTC.Meters -> Kernel.Prelude.Maybe KTC.Seconds -> [DQB.QuoteBreakup] -> [API.Types.UI.EstimateBP.EstimateBreakup]
transformOneWayRateCard mbEstimatedDistance mbEstimatedDuration breakups =
  catMaybes
    [ toItem "Base Fare" <$> find ((== "BASE_FARE") . (.title)) breakups,
      toItem "Dead Km Fare" <$> find ((== "DEAD_KILOMETER_FARE") . (.title)) breakups,
      toItem "Per Extra Km Rate" <$> bandFor "EXTRA_PER_KM_STEP_FARE_" ((.getMeters) <$> mbEstimatedDistance),
      toItem "Per Extra Min Rate" <$> bandFor "PER_MINUTE_STEP_FARE_" ((`div` 60) . (.getSeconds) <$> mbEstimatedDuration)
    ]
  where
    toItem itemTitle breakup = createEstimateBreakup breakup.price itemTitle
    bandFor prefix mbTarget =
      let bands = [(T.splitOn "_" bandRange, breakup) | breakup <- breakups, Just bandRange <- [T.stripPrefix prefix breakup.title]]
          contains target ([startTxt, endTxt], _) = maybe False (<= target) (readMaybe $ T.unpack startTxt) && maybe True (target <) (readMaybe $ T.unpack endTxt)
          contains _ _ = False
       in snd <$> ((mbTarget >>= \target -> find (contains target) bands) <|> Kernel.Prelude.listToMaybe bands)

-- | Slim JSON shape mirroring
-- 'Storage.Queries.Transformers.Estimate.EstimateBreakupItem'. Duplicated here
-- to keep the dependency direction clean (UI handler doesn't import Storage
-- transformer internals).
data EstimateBreakupItemJson = EstimateBreakupItemJson
  { title :: Text,
    price :: KTC.Price
  }
  deriving (Generic)

instance A.FromJSON EstimateBreakupItemJson

-- | Try the new estimate.breakup_list_json Clickhouse column first; fall
-- back to the legacy estimate_breakup Clickhouse table on cache miss or
-- parse failure. Lets us migrate without a flag-day.
findEstimateBreakupCH ::
  (MonadFlow m, CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m) =>
  Id.Id DE.Estimate ->
  UTCTime ->
  m [API.Types.UI.EstimateBP.EstimateBreakup]
findEstimateBreakupCH estimateId createdAt = do
  mEstimate <- CHE.findById estimateId createdAt
  let mItems = do
        e <- mEstimate
        jsonText <- e.breakupListJson
        A.decodeStrict (TE.encodeUtf8 jsonText) :: Maybe [EstimateBreakupItemJson]
  case mItems of
    Just items@(_ : _) -> pure $ map itemToApi items
    _ -> do
      breakup <- CH.findAllByEstimateIdT estimateId createdAt
      pure $ transformEstimate <$> breakup
  where
    itemToApi item =
      API.Types.UI.EstimateBP.EstimateBreakup
        { price =
            API.Types.UI.EstimateBP.EstimateBreakupPrice
              { value =
                  PriceAPIEntity
                    { amount = item.price.amount,
                      currency = item.price.currency
                    }
              },
          title = item.title
        }

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
