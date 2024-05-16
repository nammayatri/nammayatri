{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.Transformers.Quote where

import Domain.Types.DriverOffer as DDO
import Domain.Types.Estimate
import Domain.Types.FarePolicy.FareProductType as DFFP
import qualified Domain.Types.FarePolicy.FareProductType
import qualified Domain.Types.MerchantOperatingCity
import Domain.Types.Quote as DQ
import qualified Domain.Types.Quote
import Domain.Types.SearchRequest
import qualified Domain.Types.TripTerms
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics
import Kernel.Types.Common
import qualified Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Common (KvDbFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.DriverOffer as BeamDO
import qualified Storage.Beam.Quote as BeamQ
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.Queries.DriverOffer as QueryDO
import Storage.Queries.RentalDetails as QueryRD
import Storage.Queries.SpecialZoneQuote as QuerySZQ
import qualified Storage.Queries.TripTerms as QTT

getQuoteDetails' :: Domain.Types.Quote.QuoteDetails -> ((Domain.Types.FarePolicy.FareProductType.FareProductType, Kernel.Prelude.Maybe Kernel.Types.Common.Distance, Kernel.Prelude.Maybe Kernel.Prelude.Text, Kernel.Prelude.Maybe Kernel.Prelude.Text, Kernel.Prelude.Maybe Kernel.Prelude.Text))
getQuoteDetails' quoteDetails =
  let (fareProductType, distanceToNearestDriver, rentalDetailsId, driverOfferId, specialZoneQuoteId) = case quoteDetails of
        DQ.OneWayDetails details -> (DFFP.ONE_WAY, Just $ details.distanceToNearestDriver, Nothing, Nothing, Nothing)
        DQ.RentalDetails rentalDetails -> (DFFP.RENTAL, Nothing, Just $ getId rentalDetails.id, Nothing, Nothing)
        DQ.DriverOfferDetails driverOffer -> (DFFP.DRIVER_OFFER, Nothing, Nothing, Just $ getId driverOffer.id, Nothing)
        DQ.OneWaySpecialZoneDetails specialZoneQuote -> (DFFP.ONE_WAY_SPECIAL_ZONE, Nothing, Nothing, Nothing, Just $ getId specialZoneQuote.id)
        DQ.InterCityDetails details -> (DFFP.INTER_CITY, Nothing, Nothing, Nothing, Just $ getId details.id)
   in (fareProductType, distanceToNearestDriver, rentalDetailsId, driverOfferId, specialZoneQuoteId)

getQuoteDetails :: (CoreMetrics m, KvDbFlow m r, MonadReader r m) => (Domain.Types.FarePolicy.FareProductType.FareProductType -> Kernel.Prelude.Maybe HighPrecMeters -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Maybe DistanceUnit -> Maybe HighPrecDistance -> m (Domain.Types.Quote.QuoteDetails))
getQuoteDetails fareProductType distanceToNearestDriver rentalDetailsId driverOfferId specialZoneQuoteId distanceUnit distanceToNearestDriverValue = case fareProductType of
  DFFP.ONE_WAY -> do
    distanceToNearestDriver' <- (mkDistanceWithDefault distanceUnit distanceToNearestDriverValue <$> distanceToNearestDriver) & fromMaybeM (QuoteFieldNotPresent "distanceToNearestDriver")
    pure . DQ.OneWayDetails $
      DQ.OneWayQuoteDetails
        { distanceToNearestDriver = distanceToNearestDriver'
        }
  DFFP.RENTAL -> do
    qd <- getRentalDetails rentalDetailsId
    maybe (throwError (InternalError "No rental details")) return qd
  DFFP.DRIVER_OFFER -> do
    qd <- getDriverOfferDetails driverOfferId
    maybe (throwError (InternalError "No driver offer details")) return qd
  DFFP.ONE_WAY_SPECIAL_ZONE -> do
    qd <- getSpecialZoneQuote specialZoneQuoteId
    maybe (throwError (InternalError "No special zone details")) return qd
  DFFP.INTER_CITY -> do
    qd <- getInterCityQuote specialZoneQuoteId
    maybe (throwError (InternalError "No inter city details")) return qd
  where
    getRentalDetails rentalDetailsId' = do
      res <- maybe (pure Nothing) (QueryRD.findById . Id) rentalDetailsId'
      maybe (pure Nothing) (pure . Just . DQ.RentalDetails) res

    getDriverOfferDetails driverOfferId' = do
      res <- maybe (pure Nothing) (QueryDO.findById . Id) driverOfferId'
      maybe (pure Nothing) (pure . Just . DQ.DriverOfferDetails) res

    getSpecialZoneQuote specialZoneQuoteId' = do
      res <- maybe (pure Nothing) (QuerySZQ.findById . Id) specialZoneQuoteId'
      maybe (pure Nothing) (pure . Just . DQ.OneWaySpecialZoneDetails) res

    getInterCityQuote specialZoneQuoteId' = do
      res <- maybe (pure Nothing) (QuerySZQ.findById . Id) specialZoneQuoteId'
      maybe (pure Nothing) (pure . Just . DQ.InterCityDetails) res

getTripTerms :: (CoreMetrics m, KvDbFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.Text -> m (Kernel.Prelude.Maybe Domain.Types.TripTerms.TripTerms))
getTripTerms tripTermsId = if isJust tripTermsId then QTT.findById'' (Id (fromJust tripTermsId)) else pure Nothing

backfillMOCId :: KvDbFlow m r => Maybe Text -> Text -> m (Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity)
backfillMOCId mocId merchantId =
  case mocId of
    Just mocId' -> pure $ Id mocId'
    Nothing -> (.id) <$> CQM.getDefaultMerchantOperatingCity (Id merchantId)

getfareProduct :: (Domain.Types.FarePolicy.FareProductType.FareProductType, Kernel.Prelude.Maybe Kernel.Types.Common.Distance, Kernel.Prelude.Maybe Kernel.Prelude.Text, Kernel.Prelude.Maybe Kernel.Prelude.Text, Kernel.Prelude.Maybe Kernel.Prelude.Text) -> Domain.Types.FarePolicy.FareProductType.FareProductType
getfareProduct (a, _, _, _, _) = a

getDistanceToNearestDriver :: (Domain.Types.FarePolicy.FareProductType.FareProductType, Kernel.Prelude.Maybe Kernel.Types.Common.Distance, Kernel.Prelude.Maybe Kernel.Prelude.Text, Kernel.Prelude.Maybe Kernel.Prelude.Text, Kernel.Prelude.Maybe Kernel.Prelude.Text) -> Kernel.Prelude.Maybe Kernel.Types.Common.Distance
getDistanceToNearestDriver (_, a, _, _, _) = a

getRentalDetailsId :: (Domain.Types.FarePolicy.FareProductType.FareProductType, Kernel.Prelude.Maybe Kernel.Types.Common.Distance, Kernel.Prelude.Maybe Kernel.Prelude.Text, Kernel.Prelude.Maybe Kernel.Prelude.Text, Kernel.Prelude.Maybe Kernel.Prelude.Text) -> Kernel.Prelude.Maybe Kernel.Prelude.Text
getRentalDetailsId (_, _, a, _, _) = a

getDriverOfferId :: (Domain.Types.FarePolicy.FareProductType.FareProductType, Kernel.Prelude.Maybe Kernel.Types.Common.Distance, Kernel.Prelude.Maybe Kernel.Prelude.Text, Kernel.Prelude.Maybe Kernel.Prelude.Text, Kernel.Prelude.Maybe Kernel.Prelude.Text) -> Kernel.Prelude.Maybe Kernel.Prelude.Text
getDriverOfferId (_, _, _, a, _) = a

getSpecialZoneQuoteId :: (Domain.Types.FarePolicy.FareProductType.FareProductType, Kernel.Prelude.Maybe Kernel.Types.Common.Distance, Kernel.Prelude.Maybe Kernel.Prelude.Text, Kernel.Prelude.Maybe Kernel.Prelude.Text, Kernel.Prelude.Maybe Kernel.Prelude.Text) -> Kernel.Prelude.Maybe Kernel.Prelude.Text
getSpecialZoneQuoteId (_, _, _, _, a) = a

mkTollChargesInfo :: (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney -> Kernel.Prelude.Maybe [Kernel.Prelude.Text] -> Kernel.Prelude.Maybe Kernel.Types.Common.Currency -> Kernel.Prelude.Maybe Domain.Types.Quote.TollChargesInfo)
mkTollChargesInfo tollCharges tollNames currency =
  ((,) <$> tollCharges <*> tollNames)
    <&> \(tollCharges', tollNames') ->
      DQ.TollChargesInfo
        { tollCharges = mkPriceWithDefault (Just tollCharges') currency (round tollCharges' :: Money),
          tollNames = tollNames'
        }
