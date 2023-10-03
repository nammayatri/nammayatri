{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Beckn.OnSearch
  ( DOnSearchReq (..),
    ProviderInfo (..),
    EstimateInfo (..),
    DEstimate.FareRange (..),
    QuoteInfo (..),
    QuoteDetails (..),
    OneWayQuoteDetails (..),
    OneWaySpecialZoneQuoteDetails (..),
    PublicTransportQuoteDetails (..),
    RentalQuoteDetails (..),
    EstimateBreakupInfo (..),
    BreakupPriceInfo (..),
    NightShiftInfo (..),
    WaitingChargesInfo (..),
    RouteInfo (..),
    Stop (..),
    onSearch,
    validateRequest,
  )
where

import Beckn.Types.Core.Taxi.OnSearch.StartInfo (Location)
import qualified Domain.Types.Estimate as DEstimate
import qualified Domain.Types.Merchant as DMerchant
import qualified Domain.Types.Merchant.MerchantPaymentMethod as DMPM
import qualified Domain.Types.Person.PersonFlowStatus as DPFS
import qualified Domain.Types.PublicTransportQuote as DPublicTransportQuote
import qualified Domain.Types.PublicTransportQuote.RouteInfo as DPTQRouteInfo
import qualified Domain.Types.PublicTransportQuote.Stop as DPTQStop
import qualified Domain.Types.Quote as DQuote
import qualified Domain.Types.RentalSlab as DRentalSlab
import Domain.Types.SearchRequest
import qualified Domain.Types.SearchRequest as DSearchReq
import qualified Domain.Types.SpecialZoneQuote as DSpecialZoneQuote
import qualified Domain.Types.TripTerms as DTripTerms
import Domain.Types.VehicleVariant
import Environment
import Kernel.Beam.Functions
import Kernel.External.Maps hiding (RouteInfo)
import Kernel.Prelude
import Kernel.Types.Common hiding (id)
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.Merchant as QMerch
import qualified Storage.CachedQueries.Merchant.MerchantPaymentMethod as CQMPM
import qualified Storage.CachedQueries.Person.PersonFlowStatus as QPFS
import qualified Storage.Queries.Estimate as QEstimate
import qualified Storage.Queries.Quote as QQuote
import qualified Storage.Queries.SearchRequest as QSearchReq
import Tools.Error
import Tools.Event
import qualified Tools.Metrics as Metrics

data DOnSearchReq = DOnSearchReq
  { requestId :: Id DSearchReq.SearchRequest,
    providerInfo :: ProviderInfo,
    estimatesInfo :: [EstimateInfo],
    quotesInfo :: [QuoteInfo],
    paymentMethodsInfo :: [DMPM.PaymentMethodInfo]
  }

data ValidatedOnSearchReq = ValidatedOnSearchReq
  { requestId :: Id DSearchReq.SearchRequest,
    providerInfo :: ProviderInfo,
    estimatesInfo :: [EstimateInfo],
    quotesInfo :: [QuoteInfo],
    _searchRequest :: SearchRequest,
    merchant :: DMerchant.Merchant,
    paymentMethodsInfo :: [DMPM.PaymentMethodInfo]
  }

data ProviderInfo = ProviderInfo
  { providerId :: Text,
    name :: Text,
    url :: BaseUrl,
    mobileNumber :: Text,
    ridesCompleted :: Int
  }

data EstimateInfo = EstimateInfo
  { bppEstimateId :: Id DEstimate.BPPEstimate,
    vehicleVariant :: VehicleVariant,
    estimatedFare :: Money,
    discount :: Maybe Money,
    estimatedTotalFare :: Money,
    itemId :: Text,
    totalFareRange :: DEstimate.FareRange,
    descriptions :: [Text],
    estimateBreakupList :: [EstimateBreakupInfo],
    nightShiftInfo :: Maybe NightShiftInfo,
    waitingCharges :: Maybe WaitingChargesInfo,
    driversLocation :: [LatLong],
    specialLocationTag :: Maybe Text
  }

data NightShiftInfo = NightShiftInfo
  { nightShiftCharge :: Money,
    oldNightShiftCharge :: Centesimal,
    nightShiftStart :: TimeOfDay,
    nightShiftEnd :: TimeOfDay
  }

newtype WaitingChargesInfo = WaitingChargesInfo
  { waitingChargePerMin :: Maybe Money
  }

data EstimateBreakupInfo = EstimateBreakupInfo
  { title :: Text,
    price :: BreakupPriceInfo
  }

data BreakupPriceInfo = BreakupPriceInfo
  { currency :: Text,
    value :: Money
  }

data QuoteInfo = QuoteInfo
  { vehicleVariant :: VehicleVariant,
    estimatedFare :: Money,
    discount :: Maybe Money,
    estimatedTotalFare :: Money,
    quoteDetails :: QuoteDetails,
    itemId :: Text,
    descriptions :: [Text],
    specialLocationTag :: Maybe Text
  }

data RouteInfo = RouteInfo
  { routeId :: Text,
    tripId :: Text,
    routeNo :: Text,
    routeName :: Text
  }

data Stop = Stop
  { location :: Location,
    scheduledTime :: Text
  }

data QuoteDetails
  = OneWayDetails OneWayQuoteDetails
  | RentalDetails RentalQuoteDetails
  | OneWaySpecialZoneDetails OneWaySpecialZoneQuoteDetails
  | PublicTransportDetails PublicTransportQuoteDetails

newtype OneWayQuoteDetails = OneWayQuoteDetails
  { distanceToNearestDriver :: HighPrecMeters
  }

newtype OneWaySpecialZoneQuoteDetails = OneWaySpecialZoneQuoteDetails
  { quoteId :: Text
  }

data RentalQuoteDetails = RentalQuoteDetails
  { baseDistance :: Kilometers,
    baseDuration :: Hours
  }

data PublicTransportQuoteDetails = PublicTransportQuoteDetails
  { quoteId :: Text,
    start :: Stop,
    end :: Stop,
    routeInfo :: RouteInfo,
    totalEstimatedDistance :: Text,
    totalDuration :: Text,
    vehicleServiceType :: Text,
    stops :: [Stop]
  }

validateRequest :: DOnSearchReq -> Flow ValidatedOnSearchReq
validateRequest DOnSearchReq {..} = do
  _searchRequest <- runInReplica $ QSearchReq.findById requestId >>= fromMaybeM (SearchRequestDoesNotExist requestId.getId)
  merchant <- QMerch.findById _searchRequest.merchantId >>= fromMaybeM (MerchantNotFound _searchRequest.merchantId.getId)
  return $ ValidatedOnSearchReq {..}

onSearch ::
  Text ->
  ValidatedOnSearchReq ->
  Flow ()
onSearch transactionId ValidatedOnSearchReq {..} = do
  Metrics.finishSearchMetrics merchant.name transactionId
  now <- getCurrentTime

  estimates <- traverse (buildEstimate providerInfo now _searchRequest) estimatesInfo
  quotes <- traverse (buildQuote requestId providerInfo now _searchRequest.merchantId) quotesInfo
  merchantPaymentMethods <- CQMPM.findAllByMerchantId merchant.id
  let paymentMethods = intersectPaymentMethods paymentMethodsInfo merchantPaymentMethods
  forM_ estimates $ \est -> do
    triggerEstimateEvent EstimateEventData {estimate = est, personId = _searchRequest.riderId, merchantId = _searchRequest.merchantId}
  _ <- QEstimate.createMany estimates
  _ <- QQuote.createMany quotes
  _ <- QPFS.updateStatus _searchRequest.riderId DPFS.GOT_ESTIMATE {requestId = _searchRequest.id, validTill = _searchRequest.validTill}
  _ <- QSearchReq.updatePaymentMethods _searchRequest.id (paymentMethods <&> (.id))
  QPFS.clearCache _searchRequest.riderId

buildEstimate ::
  MonadFlow m =>
  ProviderInfo ->
  UTCTime ->
  SearchRequest ->
  EstimateInfo ->
  m DEstimate.Estimate
buildEstimate providerInfo now _searchRequest EstimateInfo {..} = do
  uid <- generateGUID
  tripTerms <- buildTripTerms descriptions
  estimateBreakupList' <- buildEstimateBreakUp estimateBreakupList uid
  pure
    DEstimate.Estimate
      { id = uid,
        requestId = _searchRequest.id,
        merchantId = Just _searchRequest.merchantId,
        providerMobileNumber = providerInfo.mobileNumber,
        providerName = providerInfo.name,
        providerCompletedRidesCount = providerInfo.ridesCompleted,
        providerId = providerInfo.providerId,
        providerUrl = providerInfo.url,
        estimatedDistance = _searchRequest.distance,
        estimatedDuration = _searchRequest.estimatedRideDuration,
        device = _searchRequest.device,
        createdAt = now,
        updatedAt = now,
        status = DEstimate.NEW,
        estimateBreakupList = estimateBreakupList',
        driversLocation = driversLocation,
        nightShiftInfo =
          nightShiftInfo <&> \nightShiftInfo' ->
            DEstimate.NightShiftInfo
              { nightShiftCharge = nightShiftInfo'.nightShiftCharge,
                oldNightShiftCharge = nightShiftInfo'.oldNightShiftCharge, -- TODO: Doesn't make sense, to be removed
                nightShiftStart = nightShiftInfo'.nightShiftStart,
                nightShiftEnd = nightShiftInfo'.nightShiftEnd
              },
        waitingCharges =
          DEstimate.WaitingCharges
            { waitingChargePerMin = waitingCharges >>= (.waitingChargePerMin)
            },
        ..
      }

buildQuote ::
  MonadFlow m =>
  Id DSearchReq.SearchRequest ->
  ProviderInfo ->
  UTCTime ->
  Id DMerchant.Merchant ->
  QuoteInfo ->
  m DQuote.Quote
buildQuote requestId providerInfo now merchantId QuoteInfo {..} = do
  uid <- generateGUID
  tripTerms <- buildTripTerms descriptions
  quoteDetails' <- case quoteDetails of
    OneWayDetails oneWayDetails ->
      pure.DQuote.OneWayDetails $ mkOneWayQuoteDetails oneWayDetails
    RentalDetails rentalSlab -> do
      DQuote.RentalDetails <$> buildRentalSlab rentalSlab
    OneWaySpecialZoneDetails details -> do
      DQuote.OneWaySpecialZoneDetails <$> buildOneWaySpecialZoneQuoteDetails details
    PublicTransportDetails details -> do
      DQuote.PublicTransportQuoteDetails <$> buildPublicTransportQuoteDetails now details
  pure
    DQuote.Quote
      { id = uid,
        providerMobileNumber = providerInfo.mobileNumber,
        providerName = providerInfo.name,
        providerCompletedRidesCount = providerInfo.ridesCompleted,
        providerId = providerInfo.providerId,
        providerUrl = providerInfo.url,
        createdAt = now,
        quoteDetails = quoteDetails',
        merchantId,
        ..
      }

mkOneWayQuoteDetails :: OneWayQuoteDetails -> DQuote.OneWayQuoteDetails
mkOneWayQuoteDetails OneWayQuoteDetails {..} = DQuote.OneWayQuoteDetails {..}

buildOneWaySpecialZoneQuoteDetails :: MonadFlow m => OneWaySpecialZoneQuoteDetails -> m DSpecialZoneQuote.SpecialZoneQuote
buildOneWaySpecialZoneQuoteDetails OneWaySpecialZoneQuoteDetails {..} = do
  id <- generateGUID
  pure DSpecialZoneQuote.SpecialZoneQuote {..}

buildRentalSlab :: MonadFlow m => RentalQuoteDetails -> m DRentalSlab.RentalSlab
buildRentalSlab RentalQuoteDetails {..} = do
  id <- generateGUID
  pure DRentalSlab.RentalSlab {..}

buildPublicTransportQuoteDetails :: MonadFlow m => UTCTime -> PublicTransportQuoteDetails -> m DPublicTransportQuote.PublicTransportQuote
buildPublicTransportQuoteDetails now PublicTransportQuoteDetails {..} = do
  id <- generateGUID
  start' <- buildStop' now start
  end' <- buildStop' now end
  routeInfo' <- buildRouteInfo' now routeInfo
  pure
    DPublicTransportQuote.PublicTransportQuote
      { start = start',
        end = end',
        routeInfo = routeInfo',
        stops = [], -- FIXME : use `stops` from parameter to build this field.
        createdAt = now,
        ..
      }

buildStop' :: MonadFlow m => UTCTime -> Stop -> m DPTQStop.Stop
buildStop' now Stop {..} = do
  id <- generateGUID
  pure
    DPTQStop.Stop
      { createdAt = now,
        ..
      }

buildRouteInfo' :: MonadFlow m => UTCTime -> RouteInfo -> m DPTQRouteInfo.RouteInfo
buildRouteInfo' now RouteInfo {..} = do
  id <- generateGUID
  pure
    DPTQRouteInfo.RouteInfo
      { createdAt = now,
        ..
      }

buildTripTerms ::
  MonadFlow m =>
  [Text] ->
  m (Maybe DTripTerms.TripTerms)
buildTripTerms [] = pure Nothing
buildTripTerms descriptions = do
  id <- generateGUID
  pure . Just $ DTripTerms.TripTerms {..}

buildEstimateBreakUp ::
  MonadFlow m =>
  [EstimateBreakupInfo] ->
  Id DEstimate.Estimate ->
  m [DEstimate.EstimateBreakup]
buildEstimateBreakUp estimatesItems estId =
  estimatesItems
    `for` \estimateItem -> do
      id <- generateGUID
      price' <- mkEstimatePrice estimateItem.price
      pure
        DEstimate.EstimateBreakup
          { title = estimateItem.title,
            price = price',
            estimateId = estId,
            ..
          }

mkEstimatePrice ::
  MonadFlow m =>
  BreakupPriceInfo ->
  m DEstimate.EstimateBreakupPrice
mkEstimatePrice BreakupPriceInfo {..} = pure DEstimate.EstimateBreakupPrice {..}

intersectPaymentMethods :: [DMPM.PaymentMethodInfo] -> [DMPM.MerchantPaymentMethod] -> [DMPM.MerchantPaymentMethod]
intersectPaymentMethods providerPaymentMethods = filter (\mpm -> any (compareMerchantPaymentMethod mpm) providerPaymentMethods)

compareMerchantPaymentMethod :: DMPM.MerchantPaymentMethod -> DMPM.PaymentMethodInfo -> Bool
compareMerchantPaymentMethod DMPM.MerchantPaymentMethod {..} providerPaymentMethod =
  paymentType == providerPaymentMethod.paymentType
    && paymentInstrument == providerPaymentMethod.paymentInstrument
    && collectedBy == providerPaymentMethod.collectedBy
