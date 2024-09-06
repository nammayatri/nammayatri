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
    TollChargesInfo (..),
    DEstimate.FareRange (..),
    QuoteInfo (..),
    QuoteDetails (..),
    OneWayQuoteDetails (..),
    OneWaySpecialZoneQuoteDetails (..),
    InterCityQuoteDetails (..),
    RentalQuoteDetails (..),
    QuoteBreakupInfo (..),
    EstimateBreakupInfo (..),
    BreakupPriceInfo (..),
    NightShiftInfo (..),
    WaitingChargesInfo (..),
    onSearch,
    validateRequest,
    OneWayScheduledQuoteDetails (..),
  )
where

import qualified Beckn.OnDemand.Utils.Common as Utils
import qualified BecknV2.OnDemand.Enums as Enums
import qualified Domain.Action.UI.Quote as DQ (estimateBuildLockKey)
import qualified Domain.Types as DT
import Domain.Types.BppDetails
import qualified Domain.Types.Estimate as DEstimate
import qualified Domain.Types.InterCityDetails as DInterCityDetails
import qualified Domain.Types.Merchant as DMerchant
import qualified Domain.Types.MerchantOperatingCity as DMerchantOperatingCity
import qualified Domain.Types.MerchantPaymentMethod as DMPM
import qualified Domain.Types.OneWayScheduledQuote as DScheduledQuote
import qualified Domain.Types.Quote as DQuote
import qualified Domain.Types.QuoteBreakup as DQuoteBreakup
import qualified Domain.Types.RentalDetails as DRentalDetails
import Domain.Types.SearchRequest
import qualified Domain.Types.SearchRequest as DSearchReq
import qualified Domain.Types.ServiceTierType as DVST
import qualified Domain.Types.SpecialZoneQuote as DSpecialZoneQuote
import qualified Domain.Types.TripTerms as DTripTerms
import Domain.Types.VehicleVariant
import qualified Domain.Types.VehicleVariant as DV
import Environment
import Kernel.Beam.Functions
import Kernel.External.Maps
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Tools.Metrics.CoreMetrics
import qualified Kernel.Types.Beckn.Domain as Domain
import Kernel.Types.Common hiding (id)
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.CachedQueries.BecknConfig as CQBC
import qualified Storage.CachedQueries.BppDetails as CQBppDetails
import qualified Storage.CachedQueries.Merchant as QMerch
import qualified Storage.CachedQueries.Person.PersonFlowStatus as QPFS
import qualified Storage.CachedQueries.ValueAddNP as CQVAN
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
    searchRequest :: SearchRequest,
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
    estimatedFare :: Price,
    discount :: Maybe Price,
    estimatedTotalFare :: Price,
    estimatedPickupDuration :: Maybe Seconds,
    itemId :: Text,
    totalFareRange :: DEstimate.FareRange,
    descriptions :: [Text],
    estimateBreakupList :: [EstimateBreakupInfo],
    nightShiftInfo :: Maybe NightShiftInfo,
    tollChargesInfo :: Maybe TollChargesInfo,
    waitingCharges :: Maybe WaitingChargesInfo,
    driversLocation :: [LatLong],
    specialLocationTag :: Maybe Text,
    validTill :: UTCTime,
    serviceTierName :: Maybe Text,
    serviceTierType :: Maybe DVST.ServiceTierType,
    serviceTierShortDesc :: Maybe Text,
    isCustomerPrefferedSearchRoute :: Maybe Bool,
    isBlockedRoute :: Maybe Bool,
    vehicleServiceTierAirConditioned :: Maybe Double,
    isAirConditioned :: Maybe Bool,
    vehicleServiceTierSeatingCapacity :: Maybe Int,
    specialLocationName :: Maybe Text,
    tripCategory :: DT.TripCategory,
    vehicleCategory :: Enums.VehicleCategory
  }

data NightShiftInfo = NightShiftInfo
  { nightShiftCharge :: Price,
    oldNightShiftCharge :: Maybe Centesimal,
    nightShiftStart :: TimeOfDay,
    nightShiftEnd :: TimeOfDay
  }

data TollChargesInfo = TollChargesInfo
  { tollCharges :: Price,
    tollNames :: [Text]
  }

newtype WaitingChargesInfo = WaitingChargesInfo
  { waitingChargePerMin :: Maybe Price
  }

data EstimateBreakupInfo = EstimateBreakupInfo
  { title :: Text,
    price :: BreakupPriceInfo
  }

data QuoteBreakupInfo = QuoteBreakupInfo
  { title :: Text,
    price :: BreakupPriceInfo
  }

newtype BreakupPriceInfo = BreakupPriceInfo
  { value :: Price
  }

data QuoteInfo = QuoteInfo
  { vehicleVariant :: VehicleVariant,
    estimatedFare :: Price,
    discount :: Maybe Price,
    estimatedTotalFare :: Price,
    estimatedPickupDuration :: Maybe Seconds,
    quoteDetails :: QuoteDetails,
    itemId :: Text,
    descriptions :: [Text],
    specialLocationTag :: Maybe Text,
    validTill :: UTCTime,
    serviceTierName :: Maybe Text,
    serviceTierType :: Maybe DVST.ServiceTierType,
    serviceTierShortDesc :: Maybe Text,
    isCustomerPrefferedSearchRoute :: Maybe Bool,
    isBlockedRoute :: Maybe Bool,
    tollChargesInfo :: Maybe TollChargesInfo,
    vehicleServiceTierAirConditioned :: Maybe Double,
    isAirConditioned :: Maybe Bool,
    vehicleServiceTierSeatingCapacity :: Maybe Int,
    specialLocationName :: Maybe Text,
    quoteBreakupList :: [QuoteBreakupInfo],
    tripCategory :: DT.TripCategory,
    vehicleCategory :: Enums.VehicleCategory
  }

data QuoteDetails
  = OneWayDetails OneWayQuoteDetails
  | InterCityDetails InterCityQuoteDetails
  | RentalDetails RentalQuoteDetails
  | OneWaySpecialZoneDetails OneWaySpecialZoneQuoteDetails
  | OneWayScheduledDetails OneWayScheduledQuoteDetails

newtype OneWayQuoteDetails = OneWayQuoteDetails
  { distanceToNearestDriver :: HighPrecMeters
  }

newtype OneWaySpecialZoneQuoteDetails = OneWaySpecialZoneQuoteDetails
  { quoteId :: Text
  }

newtype OneWayScheduledQuoteDetails = OneWayScheduledQuoteDetails
  { quoteId :: Text
  }

data InterCityQuoteDetails = InterCityQuoteDetails
  { quoteId :: Text,
    baseFare :: Price,
    perHourCharge :: Price,
    perExtraMinRate :: Price,
    perExtraKmRate :: Price,
    kmPerPlannedExtraHour :: Kilometers,
    plannedPerKmRateOneWay :: Price,
    plannedPerKmRateRoundTrip :: Price,
    perDayMaxHourAllowance :: Hours,
    perDayMaxAllowanceInMins :: Maybe Minutes,
    deadKmFare :: Price,
    nightShiftInfo :: Maybe NightShiftInfo
  }

data RentalQuoteDetails = RentalQuoteDetails
  { id :: Text,
    baseFare :: Price,
    perHourCharge :: Price,
    perExtraMinRate :: Price,
    includedDistancePerHr :: Kilometers,
    plannedPerKmRate :: Price,
    perExtraKmRate :: Price,
    deadKmFare :: Price,
    nightShiftInfo :: Maybe NightShiftInfo
  }

validateRequest :: DOnSearchReq -> Flow ValidatedOnSearchReq
validateRequest DOnSearchReq {..} = do
  searchRequest <- runInReplica $ QSearchReq.findById requestId >>= fromMaybeM (SearchRequestDoesNotExist requestId.getId)
  merchant <- QMerch.findById searchRequest.merchantId >>= fromMaybeM (MerchantNotFound searchRequest.merchantId.getId)
  _ <- Utils.validateSubscriber providerInfo.providerId merchant.id searchRequest.merchantOperatingCityId
  return $ ValidatedOnSearchReq {..}

onSearch ::
  Text ->
  ValidatedOnSearchReq ->
  Flow ()
onSearch transactionId ValidatedOnSearchReq {..} = do
  Metrics.finishSearchMetrics merchant.name transactionId
  now <- getCurrentTime

  mkBppDetails >>= CQBppDetails.createIfNotPresent

  isValueAddNP <- CQVAN.isValueAddNP providerInfo.providerId
  becknConfigs <- CQBC.findByMerchantIdDomainandMerchantOperatingCityId searchRequest.merchantId (show Domain.MOBILITY) searchRequest.merchantOperatingCityId
  becknConfig <- listToMaybe becknConfigs & fromMaybeM (InvalidRequest $ "BecknConfig not found for merchantId " <> show searchRequest.merchantId.getId <> " merchantOperatingCityId " <> show searchRequest.merchantOperatingCityId.getId) -- Using findAll for backward compatibility, TODO : Remove findAll and use findOne
  blackListedVehicles <- Utils.getBlackListedVehicles becknConfig.id providerInfo.providerId
  if not isValueAddNP && isJust searchRequest.disabilityTag
    then do
      logTagError "onSearch" "disability tag enabled search estimates discarded, not supported for OFF-US transactions"
      pure ()
    else do
      deploymentVersion <- asks (.version)
      -- TODO(MultiModal)
      {-
      case searchRequest.parentSearchId of
        Just parentSearchId -> do
          parentEstimates <- getEstimates parentSearchId -- getEstimates using parent searchId and check if multimodal estimate if available
          if null parentEstimates
            then do
              create estimate with parent searchId and check if allroutes estimates came if yes then mark estimate as done else mark with ongoing
            else do
              update estimates price with new estimate price
              check if allroutes estimates came if yes then mark estimate as done else mark with ongoing
        Nothing -> pure ()
      -}
      estimates <- traverse (buildEstimate providerInfo now searchRequest deploymentVersion) (filterEstimtesByPrefference estimatesInfo blackListedVehicles)
      quotes <- traverse (buildQuote requestId providerInfo now searchRequest deploymentVersion) (filterQuotesByPrefference quotesInfo blackListedVehicles)
      forM_ estimates $ \est -> do
        triggerEstimateEvent EstimateEventData {estimate = est, personId = searchRequest.riderId, merchantId = searchRequest.merchantId}
      let lockKey = DQ.estimateBuildLockKey searchRequest.id.getId
      Redis.withLockRedis lockKey 5 $ do
        QEstimate.createMany estimates
        QQuote.createMany quotes
        QPFS.clearCache searchRequest.riderId
  where
    {- Author: Hemant Mangla
      Rider quotes and estimates are filtered based on their preferences.
      Currently, riders preferring rentals receive only rental options.
      Ideally, rental options should also be available for one-way preferences, but frontend limitations prevent this.
      Once the frontend is updated for compatibility, we can extend this feature.
    -}
    filterQuotesByPrefference :: [QuoteInfo] -> [Enums.VehicleCategory] -> [QuoteInfo]
    filterQuotesByPrefference _quotesInfo blackListedVehicles =
      case searchRequest.riderPreferredOption of
        Rental -> filter (not . isNotRental) _quotesInfo
        OneWay -> filter (\quote -> isNotRental quote && isNotBlackListed blackListedVehicles quote.vehicleCategory) _quotesInfo
        _ -> filter isNotRental _quotesInfo

    filterEstimtesByPrefference :: [EstimateInfo] -> [Enums.VehicleCategory] -> [EstimateInfo]
    filterEstimtesByPrefference _estimateInfo blackListedVehicles =
      case searchRequest.riderPreferredOption of
        OneWay -> filter (\eInfo -> not (eInfo.vehicleVariant `elem` ambulanceVariants || isDeliveryEstimate eInfo) && (isNotBlackListed blackListedVehicles eInfo.vehicleCategory)) _estimateInfo
        Ambulance -> filter (\eInfo -> eInfo.vehicleVariant `elem` ambulanceVariants) _estimateInfo
        Delivery -> filter isDeliveryEstimate _estimateInfo
        _ -> []

    ambulanceVariants = [AMBULANCE_TAXI, AMBULANCE_TAXI_OXY, AMBULANCE_AC, AMBULANCE_AC_OXY, AMBULANCE_VENTILATOR]

    isDeliveryEstimate :: EstimateInfo -> Bool
    isDeliveryEstimate einfo = case einfo.tripCategory of
      DT.Delivery _ -> True
      _ -> False

    mkBppDetails :: Flow BppDetails
    mkBppDetails = do
      id <- generateGUID
      now <- getCurrentTime
      return $
        BppDetails
          { id,
            subscriberId = providerInfo.providerId,
            domain = show Domain.MOBILITY,
            name = providerInfo.name,
            supportNumber = Nothing,
            logoUrl = Nothing, -- TODO: Parse this from on_search req
            description = Nothing, -- TODO: Parse this from on_search req
            createdAt = now,
            updatedAt = now
          }

    isNotRental :: QuoteInfo -> Bool
    isNotRental quote = case quote.quoteDetails of RentalDetails _ -> False; _ -> True

    isNotBlackListed :: [Enums.VehicleCategory] -> Enums.VehicleCategory -> Bool
    isNotBlackListed blackListedVehicles vehicleCategory = vehicleCategory `notElem` blackListedVehicles

-- TODO(MultiModal): Add one more field in estimate for check if it is done or ongoing
buildEstimate ::
  MonadFlow m =>
  ProviderInfo ->
  UTCTime ->
  SearchRequest ->
  DeploymentVersion ->
  EstimateInfo ->
  m DEstimate.Estimate
buildEstimate providerInfo now searchRequest deploymentVersion EstimateInfo {..} = do
  uid <- generateGUID
  tripTerms <- buildTripTerms descriptions
  estimateBreakupList' <- buildEstimateBreakUp estimateBreakupList uid
  pure
    DEstimate.Estimate
      { id = uid,
        requestId = searchRequest.id,
        merchantId = Just searchRequest.merchantId,
        merchantOperatingCityId = Just searchRequest.merchantOperatingCityId,
        providerMobileNumber = providerInfo.mobileNumber,
        providerName = providerInfo.name,
        providerCompletedRidesCount = providerInfo.ridesCompleted,
        providerId = providerInfo.providerId,
        providerUrl = providerInfo.url,
        estimatedDistance = searchRequest.distance,
        serviceTierName = serviceTierName,
        vehicleServiceTierType = fromMaybe (DV.castVariantToServiceTier vehicleVariant) serviceTierType,
        serviceTierShortDesc = serviceTierShortDesc,
        estimatedDuration = searchRequest.estimatedRideDuration,
        estimatedStaticDuration = searchRequest.estimatedRideStaticDuration,
        device = searchRequest.device,
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
        tollChargesInfo =
          tollChargesInfo <&> \tollChargesInfo' ->
            DEstimate.TollChargesInfo
              { tollCharges = tollChargesInfo'.tollCharges,
                tollNames = tollChargesInfo'.tollNames
              },
        waitingCharges =
          DEstimate.WaitingCharges
            { waitingChargePerMin = waitingCharges >>= (.waitingChargePerMin)
            },
        clientBundleVersion = searchRequest.clientBundleVersion,
        clientSdkVersion = searchRequest.clientSdkVersion,
        clientDevice = searchRequest.clientDevice,
        clientConfigVersion = searchRequest.clientConfigVersion,
        backendConfigVersion = searchRequest.backendConfigVersion,
        backendAppVersion = Just deploymentVersion.getDeploymentVersion,
        distanceUnit = searchRequest.distanceUnit,
        tripCategory = Just tripCategory,
        ..
      }

buildQuote ::
  MonadFlow m =>
  Id DSearchReq.SearchRequest ->
  ProviderInfo ->
  UTCTime ->
  SearchRequest ->
  DeploymentVersion ->
  QuoteInfo ->
  m DQuote.Quote
buildQuote requestId providerInfo now searchRequest deploymentVersion QuoteInfo {..} = do
  uid <- generateGUID
  tripTerms <- buildTripTerms descriptions
  quoteBreakupList' <- buildQuoteBreakUp quoteBreakupList uid searchRequest.merchantId searchRequest.merchantOperatingCityId
  quoteDetails' <- case quoteDetails of
    OneWayDetails oneWayDetails ->
      pure.DQuote.OneWayDetails $ mkOneWayQuoteDetails searchRequest.distanceUnit oneWayDetails
    RentalDetails rentalDetails -> do
      DQuote.RentalDetails <$> buildRentalDetails searchRequest.distanceUnit rentalDetails
    OneWaySpecialZoneDetails details -> do
      DQuote.OneWaySpecialZoneDetails <$> buildOneWaySpecialZoneQuoteDetails details
    InterCityDetails details -> do
      DQuote.InterCityDetails <$> buildInterCityQuoteDetails searchRequest.distanceUnit searchRequest.roundTrip details
    OneWayScheduledDetails details -> do
      DQuote.OneWayScheduledDetails <$> buildOneWayScheduledQuoteDetails details
  pure
    DQuote.Quote
      { id = uid,
        providerId = providerInfo.providerId,
        providerUrl = providerInfo.url,
        createdAt = now,
        updatedAt = now,
        quoteDetails = quoteDetails',
        merchantId = searchRequest.merchantId,
        merchantOperatingCityId = searchRequest.merchantOperatingCityId,
        vehicleServiceTierType = fromMaybe (castVariantToServiceTier vehicleVariant) serviceTierType,
        clientBundleVersion = searchRequest.clientBundleVersion,
        clientSdkVersion = searchRequest.clientSdkVersion,
        clientDevice = searchRequest.clientDevice,
        clientConfigVersion = searchRequest.clientConfigVersion,
        backendConfigVersion = searchRequest.backendConfigVersion,
        backendAppVersion = Just deploymentVersion.getDeploymentVersion,
        quoteBreakupList = quoteBreakupList',
        tollChargesInfo =
          tollChargesInfo <&> \tollChargesInfo' ->
            DQuote.TollChargesInfo
              { tollCharges = tollChargesInfo'.tollCharges,
                tollNames = tollChargesInfo'.tollNames
              },
        distanceUnit = searchRequest.distanceUnit,
        tripCategory = Just tripCategory,
        ..
      }

mkOneWayQuoteDetails :: DistanceUnit -> OneWayQuoteDetails -> DQuote.OneWayQuoteDetails
mkOneWayQuoteDetails distanceUnit OneWayQuoteDetails {..} =
  DQuote.OneWayQuoteDetails
    { distanceToNearestDriver = convertHighPrecMetersToDistance distanceUnit distanceToNearestDriver,
      ..
    }

buildOneWaySpecialZoneQuoteDetails :: MonadFlow m => OneWaySpecialZoneQuoteDetails -> m DSpecialZoneQuote.SpecialZoneQuote
buildOneWaySpecialZoneQuoteDetails OneWaySpecialZoneQuoteDetails {..} = do
  id <- generateGUID
  now <- getCurrentTime
  let createdAt = now
      updatedAt = now
  pure DSpecialZoneQuote.SpecialZoneQuote {..}

buildOneWayScheduledQuoteDetails :: MonadFlow m => OneWayScheduledQuoteDetails -> m DScheduledQuote.OneWayScheduledQuote
buildOneWayScheduledQuoteDetails OneWayScheduledQuoteDetails {..} = do
  id <- generateGUID
  now <- getCurrentTime
  let createdAt = now
      updatedAt = now
  pure DScheduledQuote.OneWayScheduledQuote {..}

buildInterCityQuoteDetails :: MonadFlow m => DistanceUnit -> Maybe Bool -> InterCityQuoteDetails -> m DInterCityDetails.InterCityDetails
buildInterCityQuoteDetails distanceUnit roundTrip InterCityQuoteDetails {..} = do
  let id = Id quoteId
  now <- getCurrentTime
  let createdAt = now
      updatedAt = now
      nightShiftinfo' =
        ( \nightShiftInfo'' ->
            DRentalDetails.NightShiftInfo nightShiftInfo''.nightShiftCharge Nothing nightShiftInfo''.nightShiftStart nightShiftInfo''.nightShiftEnd
        )
          <$> nightShiftInfo
  pure
    DInterCityDetails.InterCityDetails
      { nightShiftInfo = nightShiftinfo',
        kmPerPlannedExtraHour = convertMetersToDistance distanceUnit . kilometersToMeters $ kmPerPlannedExtraHour,
        ..
      }

buildRentalDetails :: MonadFlow m => DistanceUnit -> RentalQuoteDetails -> m DRentalDetails.RentalDetails
buildRentalDetails distanceUnit RentalQuoteDetails {..} = do
  let quoteId = Id id
      nightShiftinfo' =
        ( \nightShiftInfo'' ->
            DRentalDetails.NightShiftInfo nightShiftInfo''.nightShiftCharge Nothing nightShiftInfo''.nightShiftStart nightShiftInfo''.nightShiftEnd
        )
          <$> nightShiftInfo
  pure
    DRentalDetails.RentalDetails
      { id = quoteId,
        nightShiftInfo = nightShiftinfo',
        includedDistancePerHr = convertMetersToDistance distanceUnit . kilometersToMeters $ includedDistancePerHr,
        ..
      }

buildTripTerms ::
  MonadFlow m =>
  [Text] ->
  m (Maybe DTripTerms.TripTerms)
buildTripTerms [] = pure Nothing
buildTripTerms descriptions = do
  id <- generateGUID
  now <- getCurrentTime
  let createdAt = now
      updatedAt = now
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
mkEstimatePrice BreakupPriceInfo {value} = pure DEstimate.EstimateBreakupPrice {value}

buildQuoteBreakUp ::
  MonadFlow m =>
  [QuoteBreakupInfo] ->
  Id DQuote.Quote ->
  Id DMerchant.Merchant ->
  Id DMerchantOperatingCity.MerchantOperatingCity ->
  m [DQuoteBreakup.QuoteBreakup]
buildQuoteBreakUp quotesItems quoteId merchantId merchantOperatingCityId =
  quotesItems
    `for` \quoteItem -> do
      id <- generateGUID
      now <- getCurrentTime
      pure
        DQuoteBreakup.QuoteBreakup
          { id = id,
            quoteId = quoteId.getId,
            title = quoteItem.title,
            price = quoteItem.price.value,
            merchantId = Just merchantId,
            merchantOperatingCityId = Just merchantOperatingCityId,
            createdAt = now,
            updatedAt = now,
            ..
          }
