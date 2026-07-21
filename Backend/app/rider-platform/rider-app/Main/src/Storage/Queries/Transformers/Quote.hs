module Storage.Queries.Transformers.Quote where

import qualified Data.Aeson as A
import qualified Data.Text as T
import Domain.Types.Common
import qualified Domain.Types.MerchantOperatingCity
import Domain.Types.Quote as DQ
import qualified Domain.Types.Quote
import qualified Domain.Types.QuoteBreakup as DQB
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.Queries.DriverOffer as QueryDO
import Storage.Queries.InterCityDetails as QueryICD
import qualified Storage.Queries.QuoteBreakup as QQB
import Storage.Queries.RentalDetails as QueryRD
import Storage.Queries.SpecialZoneQuote as QuerySZQ

fromQuoteDetails :: Domain.Types.Quote.QuoteDetails -> (FareProductType, Kernel.Prelude.Maybe Kernel.Types.Common.Distance, Kernel.Prelude.Maybe Kernel.Prelude.Text, Kernel.Prelude.Maybe Kernel.Prelude.Text, Kernel.Prelude.Maybe Kernel.Prelude.Text)
fromQuoteDetails quoteDetails =
  let (fareProductType, distanceToNearestDriver, rentalDetailsId, driverOfferId, specialZoneQuoteId) = case quoteDetails of
        DQ.DeliveryDetails details -> (DRIVER_OFFER, Nothing, Nothing, Just $ getId details.id, Nothing) -- for now as FareProductType deprecated, change later accordingly
        DQ.AmbulanceDetails details -> (AMBULANCE, Nothing, Nothing, Just $ getId details.id, Nothing)
        DQ.OneWayDetails details -> (ONE_WAY, Just $ details.distanceToNearestDriver, Nothing, Nothing, Just details.quoteId)
        DQ.RentalDetails rentalDetails -> (RENTAL, Nothing, Just $ getId rentalDetails.id, Nothing, Nothing)
        DQ.DriverOfferDetails driverOffer -> (DRIVER_OFFER, Nothing, Nothing, Just $ getId driverOffer.id, Nothing)
        DQ.OneWaySpecialZoneDetails specialZoneQuote -> (ONE_WAY_SPECIAL_ZONE, Nothing, Nothing, Nothing, Just $ getId specialZoneQuote.id)
        DQ.InterCityDetails details -> (INTER_CITY, Nothing, Nothing, Nothing, Just $ getId details.id)
        DQ.MeterRideDetails details -> (ONE_WAY, Nothing, Nothing, Nothing, Just details.quoteId)
        -- Reuses the rental_details table/column (same domain type) — FareProductType is
        -- deprecated backward-compat only, so RENTAL here doesn't affect real dispatch,
        -- which always goes through tripCategory instead (see toQuoteDetails below).
        DQ.EasyBookingDetails details -> (RENTAL, Nothing, Just $ getId details.id, Nothing, Nothing)
   in (fareProductType, distanceToNearestDriver, rentalDetailsId, driverOfferId, specialZoneQuoteId)

toQuoteDetails :: (CoreMetrics m, MonadFlow m, CoreMetrics m, CacheFlow m r, EsqDBFlow m r, MonadReader r m, MonadThrow m) => FareProductType -> Maybe TripCategory -> Kernel.Prelude.Maybe HighPrecMeters -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Maybe DistanceUnit -> Maybe HighPrecDistance -> m Domain.Types.Quote.QuoteDetails
toQuoteDetails fareProductType mbTripCategory distanceToNearestDriver rentalDetailsId meterRideBppQuoteId staticQuoteId driverOfferId specialZoneQuoteId distanceUnit distanceToNearestDriverValue =
  case mbTripCategory of
    Just tripCategory ->
      case tripCategory of
        OneWay OneWayRideOtp -> getSpecialZoneQuote specialZoneQuoteId >>= fromMaybeM (InternalError "No special zone details")
        CrossCity OneWayRideOtp _ -> getSpecialZoneQuote specialZoneQuoteId >>= fromMaybeM (InternalError "No special zone details")
        InterCity _ _ -> getInterCityQuote specialZoneQuoteId >>= fromMaybeM (InternalError "No inter city details")
        RideShare _ -> getInterCityQuote specialZoneQuoteId >>= fromMaybeM (InternalError "No inter city details")
        Rental _ -> getRentalDetails rentalDetailsId >>= fromMaybeM (InternalError "No rental details")
        EasyBooking _ -> getEasyBookingDetails rentalDetailsId >>= fromMaybeM (InternalError "No rental details")
        Ambulance _ -> getAmbulanceDetails driverOfferId >>= fromMaybeM (InternalError "No driver offer details")
        Delivery _ -> getDeliveryDetails driverOfferId >>= fromMaybeM (InternalError "No driver offer details")
        OneWay MeterRide -> getOneWayStaticDetails meterRideBppQuoteId & fromMaybeM (InternalError "No meter ride bpp quote details")
        OneWay OneWayOnDemandStaticOffer -> do
          distanceToNearestDriver' <- (mkDistanceWithDefault distanceUnit distanceToNearestDriverValue <$> distanceToNearestDriver) & fromMaybeM (QuoteFieldNotPresent "distanceToNearestDriver")
          getOneWayStaticQuoteDetails distanceToNearestDriver' staticQuoteId & fromMaybeM (InternalError "No static bpp quote details")
        _ -> getDriverOfferDetails driverOfferId >>= fromMaybeM (InternalError "No driver offer details")
    -- TODO :: For backward compatibility, please do not maintain this in future. `fareProductType` is replaced with `tripCategory`.
    Nothing ->
      case fareProductType of
        ONE_WAY -> do
          distanceToNearestDriver' <- (mkDistanceWithDefault distanceUnit distanceToNearestDriverValue <$> distanceToNearestDriver) & fromMaybeM (QuoteFieldNotPresent "distanceToNearestDriver")
          getOneWayStaticQuoteDetails distanceToNearestDriver' staticQuoteId & fromMaybeM (InternalError "No static bpp quote details")
        RENTAL -> getRentalDetails rentalDetailsId >>= fromMaybeM (InternalError "No rental details")
        DRIVER_OFFER -> getDriverOfferDetails driverOfferId >>= fromMaybeM (InternalError "No driver offer details")
        ONE_WAY_SPECIAL_ZONE -> getSpecialZoneQuote specialZoneQuoteId >>= fromMaybeM (InternalError "No special zone details")
        INTER_CITY -> getInterCityQuote specialZoneQuoteId >>= fromMaybeM (InternalError "No inter city details")
        AMBULANCE -> getAmbulanceDetails driverOfferId >>= fromMaybeM (InternalError "No driver offer details")
        -- Unreachable in practice (EasyBooking is new, so no legacy row lacks tripCategory),
        -- but the pattern must still be exhaustive; mirrors the RENTAL branch above.
        EASY_BOOKING -> getEasyBookingDetails rentalDetailsId >>= fromMaybeM (InternalError "No rental details")
  where
    getOneWayStaticDetails mbMterRideBppQuoteId = do
      mbMterRideBppQuoteId >>= \meterRideBppQuoteId' -> do
        pure . DQ.MeterRideDetails $
          DQ.MeterRideQuoteDetails
            { quoteId = meterRideBppQuoteId'
            }

    getOneWayStaticQuoteDetails distanceToNearestDriver' mbStaticBppQuoteId = do
      mbStaticBppQuoteId >>= \staticBppQuoteId' -> do
        pure . DQ.OneWayDetails $
          DQ.OneWayQuoteDetails
            { distanceToNearestDriver = distanceToNearestDriver',
              quoteId = staticBppQuoteId'
            }

    getRentalDetails rentalDetailsId' = do
      res <- maybe (pure Nothing) (QueryRD.findById . Id) rentalDetailsId'
      maybe (pure Nothing) (pure . Just . DQ.RentalDetails) res

    getEasyBookingDetails rentalDetailsId' = do
      res <- maybe (pure Nothing) (QueryRD.findById . Id) rentalDetailsId'
      maybe (pure Nothing) (pure . Just . DQ.EasyBookingDetails) res

    getDriverOfferDetails driverOfferId' = do
      res <- maybe (pure Nothing) (QueryDO.findById . Id) driverOfferId'
      maybe (pure Nothing) (pure . Just . DQ.DriverOfferDetails) res

    getAmbulanceDetails driverOfferId' = do
      res <- maybe (pure Nothing) (QueryDO.findById . Id) driverOfferId'
      maybe (pure Nothing) (pure . Just . DQ.AmbulanceDetails) res

    getSpecialZoneQuote specialZoneQuoteId' = do
      res <- maybe (pure Nothing) (QuerySZQ.findById . Id) specialZoneQuoteId'
      maybe (pure Nothing) (pure . Just . DQ.OneWaySpecialZoneDetails) res

    getInterCityQuote specialZoneQuoteId' = do
      case specialZoneQuoteId' of
        Just quoteId -> do
          mbInterCityDetails <- QueryICD.findById (Id quoteId)
          maybe (pure Nothing) (pure . Just . DQ.InterCityDetails) mbInterCityDetails
        Nothing -> pure Nothing

    getDeliveryDetails driverOfferId' = do
      res <- maybe (pure Nothing) (QueryDO.findById . Id) driverOfferId'
      maybe (pure Nothing) (pure . Just . DQ.DeliveryDetails) res

backfillMOCId :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => Maybe Text -> Text -> m (Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity)
backfillMOCId mocId merchantId =
  case mocId of
    Just mocId' -> pure $ Id mocId'
    Nothing -> (.id) <$> CQM.getDefaultMerchantOperatingCity (Id merchantId)

getfareProduct :: (FareProductType, Kernel.Prelude.Maybe Kernel.Types.Common.Distance, Kernel.Prelude.Maybe Kernel.Prelude.Text, Kernel.Prelude.Maybe Kernel.Prelude.Text, Kernel.Prelude.Maybe Kernel.Prelude.Text) -> FareProductType
getfareProduct (a, _, _, _, _) = a

getDistanceToNearestDriver :: (FareProductType, Kernel.Prelude.Maybe Kernel.Types.Common.Distance, Kernel.Prelude.Maybe Kernel.Prelude.Text, Kernel.Prelude.Maybe Kernel.Prelude.Text, Kernel.Prelude.Maybe Kernel.Prelude.Text) -> Kernel.Prelude.Maybe Kernel.Types.Common.Distance
getDistanceToNearestDriver (_, a, _, _, _) = a

getRentalDetailsId :: (FareProductType, Kernel.Prelude.Maybe Kernel.Types.Common.Distance, Kernel.Prelude.Maybe Kernel.Prelude.Text, Kernel.Prelude.Maybe Kernel.Prelude.Text, Kernel.Prelude.Maybe Kernel.Prelude.Text) -> Kernel.Prelude.Maybe Kernel.Prelude.Text
getRentalDetailsId (_, _, a, _, _) = a

getDriverOfferId :: (FareProductType, Kernel.Prelude.Maybe Kernel.Types.Common.Distance, Kernel.Prelude.Maybe Kernel.Prelude.Text, Kernel.Prelude.Maybe Kernel.Prelude.Text, Kernel.Prelude.Maybe Kernel.Prelude.Text) -> Kernel.Prelude.Maybe Kernel.Prelude.Text
getDriverOfferId (_, _, _, a, _) = a

getMeterRideBppQuoteId :: (FareProductType, Kernel.Prelude.Maybe Kernel.Types.Common.Distance, Kernel.Prelude.Maybe Kernel.Prelude.Text, Kernel.Prelude.Maybe Kernel.Prelude.Text, Kernel.Prelude.Maybe Kernel.Prelude.Text) -> Kernel.Prelude.Maybe Kernel.Prelude.Text
getMeterRideBppQuoteId (ONE_WAY, _, _, _, a) = a
getMeterRideBppQuoteId _ = Nothing

getStaticBppQuoteId :: (FareProductType, Kernel.Prelude.Maybe Kernel.Types.Common.Distance, Kernel.Prelude.Maybe Kernel.Prelude.Text, Kernel.Prelude.Maybe Kernel.Prelude.Text, Kernel.Prelude.Maybe Kernel.Prelude.Text) -> Kernel.Prelude.Maybe Kernel.Prelude.Text
getStaticBppQuoteId (ONE_WAY, _, _, _, a) = a
getStaticBppQuoteId _ = Nothing

getSpecialZoneQuoteId :: (FareProductType, Kernel.Prelude.Maybe Kernel.Types.Common.Distance, Kernel.Prelude.Maybe Kernel.Prelude.Text, Kernel.Prelude.Maybe Kernel.Prelude.Text, Kernel.Prelude.Maybe Kernel.Prelude.Text) -> Kernel.Prelude.Maybe Kernel.Prelude.Text
getSpecialZoneQuoteId (_, _, _, _, a) = a

mkTollChargesInfo :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney -> Kernel.Prelude.Maybe [Kernel.Prelude.Text] -> Kernel.Prelude.Maybe Kernel.Types.Common.Currency -> Kernel.Prelude.Maybe Domain.Types.Quote.TollChargesInfo
mkTollChargesInfo tollCharges tollNames currency =
  ((,) <$> tollCharges <*> tollNames)
    <&> \(tollCharges', tollNames') ->
      DQ.TollChargesInfo
        { tollCharges = mkPriceWithDefault (Just tollCharges') currency (round tollCharges' :: Money),
          tollNames = tollNames'
        }

-- | Slim shape persisted in the quote.quote_breakup_list_json column.
-- The full domain 'QuoteBreakup' carries id, quoteId, merchant/mocId, and
-- timestamps which are regenerated from the parent on read; only title and
-- price round-trip through JSON.
data QuoteBreakupItem = QuoteBreakupItem
  { title :: Text,
    price :: Price
  }
  deriving (Generic, Show)

instance A.ToJSON QuoteBreakupItem

instance A.FromJSON QuoteBreakupItem

-- | toTType: serialise the in-memory list into the new JSON column.
-- Returns Nothing for an empty list so the column stays NULL.
encodeQuoteBreakupList :: [DQB.QuoteBreakup] -> Maybe A.Value
encodeQuoteBreakupList [] = Nothing
encodeQuoteBreakupList xs = Just . A.toJSON $ map toItem xs
  where
    toItem qb = QuoteBreakupItem {title = qb.title, price = qb.price}

-- | fromTType loader: prefer the JSON column; fall back to the legacy
-- quote_breakup table query when the column is NULL or fails to parse.
-- 'now' is used to fill the synthetic createdAt/updatedAt on items
-- materialised from the JSON column; downstream consumers in this codebase
-- only read title and price.
loadQuoteBreakupList ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  Maybe A.Value ->
  Text ->
  m [DQB.QuoteBreakup]
loadQuoteBreakupList Nothing qId = QQB.findAllByQuoteIdT qId
loadQuoteBreakupList (Just v) qId =
  case A.fromJSON v :: A.Result [QuoteBreakupItem] of
    A.Success items -> do
      now <- getCurrentTime
      pure $ zipWith (fromItem now) [0 :: Int ..] items
    A.Error _ -> QQB.findAllByQuoteIdT qId
  where
    fromItem now idx item =
      DQB.QuoteBreakup
        { id = Id (qId <> "-bi-" <> T.pack (show idx)),
          quoteId = qId,
          title = item.title,
          price = item.price,
          merchantId = Nothing,
          merchantOperatingCityId = Nothing,
          createdAt = now,
          updatedAt = now
        }
