module Storage.Queries.Transformers.Quote where

import Domain.Types.Common
import qualified Domain.Types.MerchantOperatingCity
import Domain.Types.Quote as DQ
import qualified Domain.Types.Quote
import qualified Domain.Types.TripTerms
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.Queries.DriverOffer as QueryDO
import Storage.Queries.InterCityDetails as QueryICD
import qualified Storage.Queries.OneWayScheduledQuote as QScheduled
import Storage.Queries.RentalDetails as QueryRD
import Storage.Queries.SpecialZoneQuote as QuerySZQ
import qualified Storage.Queries.TripTerms as QTT

fromQuoteDetails :: Domain.Types.Quote.QuoteDetails -> (FareProductType, Kernel.Prelude.Maybe Kernel.Types.Common.Distance, Kernel.Prelude.Maybe Kernel.Prelude.Text, Kernel.Prelude.Maybe Kernel.Prelude.Text, Kernel.Prelude.Maybe Kernel.Prelude.Text)
fromQuoteDetails quoteDetails =
  -- TODO :: For backward compatibility, please do not maintain this in future. `fareProductType` is replaced with `tripCategory`.
  let (fareProductType, distanceToNearestDriver, rentalDetailsId, driverOfferId, specialZoneQuoteId) = case quoteDetails of
        DQ.DeliveryDetails details -> (DRIVER_OFFER, Nothing, Nothing, Just $ getId details.id, Nothing)
        DQ.AmbulanceDetails details -> (AMBULANCE, Nothing, Nothing, Just $ getId details.id, Nothing)
        DQ.OneWayDetails details -> (ONE_WAY, Just $ details.distanceToNearestDriver, Nothing, Nothing, Nothing)
        DQ.OneWayScheduledDetails details -> (ONE_WAY_SPECIAL_ZONE, Nothing, Nothing, Nothing, Just $ getId details.id)
        DQ.RentalDetails rentalDetails -> (RENTAL, Nothing, Just $ getId rentalDetails.id, Nothing, Nothing)
        DQ.DriverOfferDetails driverOffer -> (DRIVER_OFFER, Nothing, Nothing, Just $ getId driverOffer.id, Nothing)
        DQ.OneWaySpecialZoneDetails specialZoneQuote -> (ONE_WAY_SPECIAL_ZONE, Nothing, Nothing, Nothing, Just $ getId specialZoneQuote.id)
        DQ.InterCityDetails details -> (INTER_CITY, Nothing, Nothing, Nothing, Just $ getId details.id)
   in (fareProductType, distanceToNearestDriver, rentalDetailsId, driverOfferId, specialZoneQuoteId)

toQuoteDetails :: (CoreMetrics m, MonadFlow m, CoreMetrics m, CacheFlow m r, EsqDBFlow m r, MonadReader r m, MonadThrow m) => FareProductType -> Maybe TripCategory -> Kernel.Prelude.Maybe HighPrecMeters -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Maybe DistanceUnit -> Maybe HighPrecDistance -> m Domain.Types.Quote.QuoteDetails
toQuoteDetails fareProductType mbTripCategory distanceToNearestDriver rentalDetailsId driverOfferId specialZoneQuoteId distanceUnit distanceToNearestDriverValue =
  case mbTripCategory of
    Just tripCategory ->
      case tripCategory of
        OneWay OneWayRideOtp -> getSpecialZoneQuote specialZoneQuoteId >>= fromMaybeM (InternalError "No special zone details")
        OneWay OneWayOnDemandStaticOffer -> getOneWayScheduledDetails specialZoneQuoteId >>= fromMaybeM (InternalError "No one way scheduled details")
        InterCity _ _ -> getInterCityQuote specialZoneQuoteId >>= fromMaybeM (InternalError "No inter city details")
        RideShare _ -> getInterCityQuote specialZoneQuoteId >>= fromMaybeM (InternalError "No inter city details")
        Rental _ -> getRentalDetails rentalDetailsId >>= fromMaybeM (InternalError "No rental details")
        Ambulance _ -> getAmbulanceDetails driverOfferId >>= fromMaybeM (InternalError "No driver offer details")
        Delivery _ -> getDeliveryDetails driverOfferId >>= fromMaybeM (InternalError "No driver offer details")
        _ -> getDriverOfferDetails driverOfferId >>= fromMaybeM (InternalError "No driver offer details")
    -- TODO :: For backward compatibility, please do not maintain this in future. `fareProductType` is replaced with `tripCategory`.
    Nothing ->
      case fareProductType of
        ONE_WAY -> do
          distanceToNearestDriver' <- (mkDistanceWithDefault distanceUnit distanceToNearestDriverValue <$> distanceToNearestDriver) & fromMaybeM (QuoteFieldNotPresent "distanceToNearestDriver")
          pure . DQ.OneWayDetails $
            DQ.OneWayQuoteDetails
              { distanceToNearestDriver = distanceToNearestDriver'
              }
        RENTAL -> getRentalDetails rentalDetailsId >>= fromMaybeM (InternalError "No rental details")
        DRIVER_OFFER -> getDriverOfferDetails driverOfferId >>= fromMaybeM (InternalError "No driver offer details")
        ONE_WAY_SPECIAL_ZONE -> getSpecialZoneQuote specialZoneQuoteId >>= fromMaybeM (InternalError "No special zone details")
        INTER_CITY -> getInterCityQuote specialZoneQuoteId >>= fromMaybeM (InternalError "No inter city details")
        AMBULANCE -> getAmbulanceDetails driverOfferId >>= fromMaybeM (InternalError "No driver offer details")
  where
    getRentalDetails rentalDetailsId' = do
      res <- maybe (pure Nothing) (QueryRD.findById . Id) rentalDetailsId'
      maybe (pure Nothing) (pure . Just . DQ.RentalDetails) res

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

    getOneWayScheduledDetails specialZoneQuoteId' = do
      res <- maybe (pure Nothing) (QScheduled.findById . Id) specialZoneQuoteId'
      maybe (pure Nothing) (pure . Just . DQ.OneWayScheduledDetails) res

getTripTerms :: (CoreMetrics m, MonadFlow m, CoreMetrics m, CacheFlow m r, EsqDBFlow m r, MonadReader r m) => Kernel.Prelude.Maybe Kernel.Prelude.Text -> m (Kernel.Prelude.Maybe Domain.Types.TripTerms.TripTerms)
getTripTerms tripTermsId = if isJust tripTermsId then QTT.findById'' (Id (fromJust tripTermsId)) else pure Nothing

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
