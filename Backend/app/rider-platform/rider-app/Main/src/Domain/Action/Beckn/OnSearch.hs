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
    MeterRideQuoteDetails (..),
    RentalQuoteDetails (..),
    QuoteBreakupInfo (..),
    EstimateBreakupInfo (..),
    BreakupPriceInfo (..),
    NightShiftInfo (..),
    WaitingChargesInfo (..),
    onSearch,
    validateRequest,
    BusinessDiscountInfo (..),
    PersonalDiscountInfo (..),
  )
where

import qualified API.UI.Select as DSelect
import qualified Beckn.OnDemand.Utils.Common as Utils
import qualified BecknV2.OnDemand.Enums as Enums
import Data.Aeson as A
import qualified Data.Aeson
import Data.List (sortBy)
import Data.Maybe ()
import Data.Ord (comparing)
import qualified Domain.Action.UI.Confirm as DConfirm
import qualified Domain.Action.UI.Quote as DQ (estimateBuildLockKey)
import qualified Domain.Types as DT
import Domain.Types.BppDetails
import qualified Domain.Types.Estimate as DEstimate
import qualified Domain.Types.EstimateStatus as DEstimate
import qualified Domain.Types.InterCityDetails as DInterCityDetails
import qualified Domain.Types.Merchant as DMerchant
import qualified Domain.Types.MerchantOperatingCity as DMerchantOperatingCity
import qualified Domain.Types.MerchantPaymentMethod as DMPM
import qualified Domain.Types.NyRegularInstanceLog as DNyRegularInstanceLog
import qualified Domain.Types.NyRegularSubscription as NyRegularSubscription
import qualified Domain.Types.Person as Person
import qualified Domain.Types.Quote as DQuote
import qualified Domain.Types.QuoteBreakup as DQuoteBreakup
import qualified Domain.Types.RentalDetails as DRentalDetails
import qualified Domain.Types.RiderConfig as DRiderConfig
import qualified Domain.Types.RiderPreferredOption as DRPO
import Domain.Types.SearchRequest
import qualified Domain.Types.SearchRequest as DSearchReq
import qualified Domain.Types.ServiceTierType as DVST
import qualified Domain.Types.SpecialZoneQuote as DSpecialZoneQuote
import qualified Domain.Types.TripTerms as DTripTerms
import Domain.Types.VehicleVariant
import qualified Domain.Types.VehicleVariant as DV
import Environment
import Kernel.External.Maps
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Tools.Metrics.CoreMetrics
import qualified Kernel.Types.Beckn.Domain as Domain
import Kernel.Types.Common hiding (id)
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Yudhishthira.Tools.DebugLog as LYDL
-- import qualified Lib.Yudhishthira.Tools.Utils as LYTU
import qualified Lib.Yudhishthira.Types as LYT
import qualified SharedLogic.CallBPPInternal as Est
import qualified SharedLogic.CreateFareForMultiModal as SLCF
import qualified SharedLogic.EstimateTags as SEST
import qualified SharedLogic.Search as SLS
import qualified SharedLogic.Type as SLT
import Storage.CachedQueries.BecknConfig as CQBC
import qualified Storage.CachedQueries.BppDetails as CQBppDetails
import qualified Storage.CachedQueries.InsuranceConfig as CQInsuranceConfig
import qualified Storage.CachedQueries.Merchant as QMerch
import qualified Storage.CachedQueries.Merchant.RiderConfig as QRC
import qualified Storage.CachedQueries.Person.PersonFlowStatus as QPFS
import qualified Storage.CachedQueries.ValueAddNP as CQVAN
import qualified Storage.Queries.Estimate as QEstimate
import qualified Storage.Queries.NyRegularInstanceLog as QNyRegularInstanceLog
import qualified Storage.Queries.NyRegularSubscription as QNyRegularSubscription
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Quote as QQuote
import qualified Storage.Queries.SearchRequest as QSearchReq
import qualified Tools.DynamicLogic as DynamicLogic
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
    businessDiscountInfo :: Maybe BusinessDiscountInfo,
    personalDiscountInfo :: Maybe PersonalDiscountInfo,
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
    vehicleCategory :: Enums.VehicleCategory,
    vehicleIconUrl :: Maybe BaseUrl,
    tipOptions :: Maybe [Int],
    qar :: Maybe Double,
    -- petCharges :: Maybe Price,
    smartTipSuggestion :: Maybe HighPrecMoney,
    smartTipReason :: Maybe Text
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

data BusinessDiscountInfo = BusinessDiscountInfo
  { businessDiscount :: Price,
    businessDiscountPercentage :: Double
  }

data PersonalDiscountInfo = PersonalDiscountInfo
  { personalDiscount :: Price,
    personalDiscountPercentage :: Double
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
    -- petCharges :: Maybe Price,
    vehicleCategory :: Enums.VehicleCategory,
    vehicleIconUrl :: Maybe BaseUrl
  }

data QuoteDetails
  = OneWayDetails OneWayQuoteDetails
  | InterCityDetails InterCityQuoteDetails
  | RentalDetails RentalQuoteDetails
  | OneWaySpecialZoneDetails OneWaySpecialZoneQuoteDetails
  | MeterRideDetails MeterRideQuoteDetails

data MeterRideQuoteDetails = MeterRideQuoteDetails
  { quoteId :: Text
  }

data OneWayQuoteDetails = OneWayQuoteDetails
  { distanceToNearestDriver :: HighPrecMeters,
    quoteId :: Text
  }

newtype OneWaySpecialZoneQuoteDetails = OneWaySpecialZoneQuoteDetails
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

validateRequest :: DOnSearchReq -> DSearchReq.SearchRequest -> Flow ValidatedOnSearchReq
validateRequest DOnSearchReq {..} searchRequest = do
  merchant <- QMerch.findById searchRequest.merchantId >>= fromMaybeM (MerchantNotFound searchRequest.merchantId.getId)
  return $ ValidatedOnSearchReq {..}

onSearch ::
  Text ->
  ValidatedOnSearchReq ->
  Flow ()
onSearch transactionId ValidatedOnSearchReq {..} = do
  Metrics.finishSearchMetrics merchant.name transactionId
  now <- getCurrentTime

  mkBppDetails >>= CQBppDetails.createIfNotPresent
  riderConfig <- QRC.findByMerchantOperatingCityId (cast searchRequest.merchantOperatingCityId) Nothing
  let isReservedSearch = isReservedRideSearch searchRequest
  mbNySubscription <- getNyRegularSubs isReservedSearch
  isValueAddNP <- CQVAN.isValueAddNP providerInfo.providerId
  becknConfigs <- CQBC.findByMerchantIdDomainandMerchantOperatingCityId searchRequest.merchantId (show Domain.MOBILITY) searchRequest.merchantOperatingCityId
  becknConfig <- (listToMaybe becknConfigs) & fromMaybeM (InvalidRequest $ "BecknConfig not found for merchantId " <> show searchRequest.merchantId.getId <> " merchantOperatingCityId " <> show searchRequest.merchantOperatingCityId.getId) -- Using findAll for backward compatibility, TODO : Remove findAll and use findOne
  blackListedVehicles <- Utils.getBlackListedVehicles becknConfig.id providerInfo.providerId
  if not isValueAddNP && isJust searchRequest.disabilityTag
    then do
      logTagError "onSearch" "disability tag enabled search estimates discarded, not supported for OFF-US transactions"
      pure ()
    else do
      deploymentVersion <- asks (.version)
      person <- QP.findById searchRequest.riderId >>= fromMaybeM (PersonDoesNotExist searchRequest.riderId.getId)
      localTime <- getLocalCurrentTime 19800
      estimates' <- traverse (buildEstimate providerInfo now searchRequest deploymentVersion (fold ((Just . (.boostSearchPreSelectionServiceTierConfig)) =<< riderConfig))) (filterEstimtesByPrefference estimatesInfo blackListedVehicles mbNySubscription) -- add to SR
      autoPrice <- getAutoPrice estimates'
      nonACPrice <- getNonACPrice estimates'
      cabQar <- getCabQar estimates'
      autoQar <- getAutoQar estimates'
      let userType = case personVehicleCategory person of
            Just Enums.AUTO_RICKSHAW -> Just SEST.AUTO
            Just Enums.CAB -> Just SEST.CAB
            _ -> Nothing

      -- let estimateTagsData = map (getEstimateTagsData autoQar cabQar autoPrice nonACPrice userType) estimates
      estimates <- traverse (getTaggedEstimate autoQar cabQar autoPrice nonACPrice userType localTime searchRequest.merchantOperatingCityId) estimates'
      quotes <- traverse (buildQuote requestId providerInfo now searchRequest deploymentVersion) (filterQuotesByPrefference quotesInfo blackListedVehicles mbNySubscription)
      updateRiderPreferredOption quotes
      let mbRequiredEstimate = sortBy (comparing ((DEstimate.minFare . DEstimate.totalFareRange) <&> (.amount)) <> comparing ((DEstimate.maxFare . DEstimate.totalFareRange) <&> (.amount))) listToMaybe estimates
      forM_ estimates $ \est -> do
        triggerEstimateEvent EstimateEventData {estimate = est, personId = searchRequest.riderId, merchantId = searchRequest.merchantId}
      let lockKey = DQ.estimateBuildLockKey searchRequest.id.getId
      Redis.withLockRedis lockKey 5 $ do
        QEstimate.createMany estimates
        QQuote.createMany quotes
        QPFS.clearCache searchRequest.riderId

      when (searchRequest.isMeterRideSearch == Just True) $ do
        quoteForMeterRide <- (listToMaybe quotes) & fromMaybeM (InvalidRequest "Quote for meter ride doesn't exist")
        void $ DConfirm.confirm searchRequest.riderId quoteForMeterRide.id Nothing Nothing Nothing Nothing False

      whenJust mbRequiredEstimate $ \requiredEstimate -> do
        shouldAutoSelect <- SLCF.createFares requestId.getId requiredEstimate.id.getId
        let shouldAutoSelectForReserved = isReservedSearch && isJust mbNySubscription
            shouldAutoSelectFinal = shouldAutoSelect || shouldAutoSelectForReserved

        when shouldAutoSelectForReserved $ do
          logTagInfo "onSearch" $ "Auto-selecting estimate for reserved ride: " <> show searchRequest.id.getId
          -- Update NyRegularInstanceLog status to AUTO_SELECTED
          void $
            fork "Update NyRegularInstanceLog status" $
              updateNyRegularInstanceLogStatus searchRequest.id.getId

        when shouldAutoSelectFinal $ autoSelectEstimate searchRequest.riderId requiredEstimate.id
  where
    isReservedRideSearch :: DSearchReq.SearchRequest -> Bool
    isReservedRideSearch searchReq =
      case searchReq.searchMode of
        Just DSearchReq.RESERVE -> True
        _ -> False

    autoSelectEstimate personId estimateId = do
      let selectReq =
            DSelect.DSelectReq
              { customerExtraFee = Nothing,
                isPetRide = Nothing,
                customerExtraFeeWithCurrency = Nothing,
                autoAssignEnabled = True,
                autoAssignEnabledV2 = Just True,
                paymentMethodId = Nothing,
                paymentInstrument = Nothing,
                otherSelectedEstimates = Nothing,
                isAdvancedBookingEnabled = Nothing,
                deliveryDetails = Nothing,
                disabilityDisable = Nothing,
                billingCategory = Nothing,
                preferSafetyPlus = Nothing
              }
      void $ DSelect.select2' (personId, merchant.id) estimateId selectReq
    {- Author: Hemant Mangla
      Rider quotes and estimates are filtered based on their preferences.
      Currently, riders preferring rentals receive only rental options.
      Ideally, rental options should also be available for one-way preferences, but frontend limitations prevent this.
      Once the frontend is updated for compatibility, we can extend this feature.
    -}
    filterQuotesByPrefference :: [QuoteInfo] -> [Enums.VehicleCategory] -> Maybe NyRegularSubscription.NyRegularSubscription -> [QuoteInfo]
    filterQuotesByPrefference _quotesInfo blackListedVehicles mbNySubscription = do
      let filtersPreferenceQuote = filterQuotesByPrefference' _quotesInfo blackListedVehicles
      filter (\estOrQ -> maybe True (\nySubs -> maybe False (\metaData -> estOrQ.serviceTierName == metaData.vehicleServiceTierName) $ parseMetaDataFromSubs nySubs.metadata) mbNySubscription) $ filtersPreferenceQuote

    filterQuotesByPrefference' :: [QuoteInfo] -> [Enums.VehicleCategory] -> [QuoteInfo]
    filterQuotesByPrefference' _quotesInfo blackListedVehicles =
      case searchRequest.riderPreferredOption of
        DRPO.Rental -> filter (\qInfo -> not (qInfo.vehicleVariant `elem` ambulanceVariants) && (not $ isNotRental qInfo)) _quotesInfo
        DRPO.OneWay -> filter (\quote -> isNotRental quote && isNotBlackListed blackListedVehicles quote.vehicleCategory && not (quote.vehicleVariant `elem` ambulanceVariants)) _quotesInfo
        DRPO.Ambulance -> filter (\qInfo -> qInfo.vehicleVariant `elem` ambulanceVariants) _quotesInfo
        DRPO.Delivery -> []
        _ -> filter isNotRental _quotesInfo

    filterEstimtesByPrefference :: [EstimateInfo] -> [Enums.VehicleCategory] -> Maybe NyRegularSubscription.NyRegularSubscription -> [EstimateInfo]
    filterEstimtesByPrefference _estimateInfo blackListedVehicles mbNySubscription = do
      let filtersPreferenceEst = filterEstimtesByPrefference' _estimateInfo blackListedVehicles
      filter (\estOrQ -> maybe True (\nySubs -> maybe False (\metaData -> estOrQ.serviceTierName == metaData.vehicleServiceTierName) $ parseMetaDataFromSubs nySubs.metadata) mbNySubscription) $ filtersPreferenceEst

    filterEstimtesByPrefference' :: [EstimateInfo] -> [Enums.VehicleCategory] -> [EstimateInfo]
    filterEstimtesByPrefference' _estimateInfo blackListedVehicles =
      case searchRequest.riderPreferredOption of
        DRPO.OneWay -> filter (\eInfo -> not (eInfo.vehicleVariant `elem` ambulanceVariants || isDeliveryEstimate eInfo) && (isNotBlackListed blackListedVehicles eInfo.vehicleCategory)) _estimateInfo
        DRPO.InterCity -> filter (\eInfo -> not (eInfo.vehicleVariant `elem` ambulanceVariants || isDeliveryEstimate eInfo) && (isNotBlackListed blackListedVehicles eInfo.vehicleCategory)) _estimateInfo
        DRPO.Ambulance -> filter (\eInfo -> eInfo.vehicleVariant `elem` ambulanceVariants) _estimateInfo
        DRPO.Delivery -> filter isDeliveryEstimate _estimateInfo
        _ -> []

    parseMetaDataFromSubs mbMetaData =
      (\val -> case Data.Aeson.fromJSON val of Data.Aeson.Success (x :: Est.BppEstimate) -> Just x; Data.Aeson.Error _ -> Nothing) =<< mbMetaData

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

    updateRiderPreferredOption quotes = case listToMaybe quotes of
      Just quote -> do
        let actualRiderPreferredOption = case quote.quoteDetails of
              DQuote.InterCityDetails _ -> DRPO.InterCity
              _ -> DRPO.OneWay
        when (actualRiderPreferredOption == DRPO.InterCity && searchRequest.riderPreferredOption /= actualRiderPreferredOption) $ QSearchReq.updateRiderPreferredOption DRPO.InterCity quote.requestId
      _ -> pure ()

    updateNyRegularInstanceLogStatus :: Text -> Flow ()
    updateNyRegularInstanceLogStatus instanceTransactionId = do
      logTagInfo "onSearch" $ "Updating NyRegularInstanceLog status to AUTO_SELECTED for instanceTransactionId: " <> instanceTransactionId
      void $ QNyRegularInstanceLog.updateStatusByInstanceTransactionId DNyRegularInstanceLog.AUTO_SELECTED instanceTransactionId

    getNyRegularSubs :: Bool -> Flow (Maybe NyRegularSubscription.NyRegularSubscription)
    getNyRegularSubs isReservedSearch = do
      if isReservedSearch
        then do
          nyTransaction <- QNyRegularInstanceLog.findByInstanceTransactionId searchRequest.id.getId
          maybe (pure Nothing) QNyRegularSubscription.findById $ nyTransaction <&> (.nyRegularSubscriptionId)
        else pure Nothing

-- TODO(MultiModal): Add one more field in estimate for check if it is done or ongoing
buildEstimate ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  ProviderInfo ->
  UTCTime ->
  SearchRequest ->
  DeploymentVersion ->
  [DRiderConfig.VehicleServiceTierOrderConfig] ->
  EstimateInfo ->
  m DEstimate.Estimate
buildEstimate providerInfo now searchRequest deploymentVersion boostSearchPreSelectionServiceTiersConfig EstimateInfo {..} = do
  uid <- generateGUID
  tripTerms <- buildTripTerms descriptions
  let vehicleServiceTierType = fromMaybe (DV.castVariantToServiceTier vehicleVariant) serviceTierType
  let boostSearchPreSelectionServiceTier = find (\boostSearchConfig -> boostSearchConfig.vehicle == vehicleServiceTierType) boostSearchPreSelectionServiceTiersConfig
  estimateBreakupList' <- buildEstimateBreakUp estimateBreakupList uid
  insuranceConfig <- CQInsuranceConfig.getInsuranceConfig searchRequest.merchantId searchRequest.merchantOperatingCityId tripCategory (DV.castVehicleVariantToVehicleCategory vehicleVariant)
  let isInsured = maybe False (\inc -> case inc.allowedVehicleServiceTiers of Just allowedTiers -> fromMaybe (DV.castVariantToServiceTier vehicleVariant) serviceTierType `elem` allowedTiers; Nothing -> True) insuranceConfig
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
        vehicleServiceTierType = vehicleServiceTierType,
        serviceTierShortDesc = serviceTierShortDesc,
        estimatedDuration = searchRequest.estimatedRideDuration,
        estimatedStaticDuration = searchRequest.estimatedRideStaticDuration,
        device = searchRequest.device,
        createdAt = now,
        updatedAt = now,
        status = DEstimate.NEW,
        estimateBreakupList = estimateBreakupList',
        driversLocation = driversLocation,
        businessDiscountInfo = businessDiscountInfo <&> \businessDiscountInfo' -> DEstimate.BusinessDiscountInfo {businessDiscount = businessDiscountInfo'.businessDiscount, businessDiscountPercentage = businessDiscountInfo'.businessDiscountPercentage},
        personalDiscountInfo = personalDiscountInfo <&> \personalDiscountInfo' -> DEstimate.PersonalDiscountInfo {personalDiscount = personalDiscountInfo'.personalDiscount, personalDiscountPercentage = personalDiscountInfo'.personalDiscountPercentage},
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
        insuredAmount = insuranceConfig >>= (.insuredAmount),
        isMultimodalSearch = searchRequest.isMultimodalSearch,
        boostSearchPreSelectionServiceTierConfig = (Just . (.orderArray)) =<< boostSearchPreSelectionServiceTier,
        vehicleCategory = Just vehicleCategory,
        qar = qar,
        estimateTags = Nothing,
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
    MeterRideDetails details -> do
      DQuote.MeterRideDetails <$> buildMeterRideQuoteDetails details
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
        isSafetyPlus = False,
        billingCategory = SLT.PERSONAL,
        ..
      }

buildMeterRideQuoteDetails :: MonadFlow m => MeterRideQuoteDetails -> m DQuote.MeterRideQuoteDetails
buildMeterRideQuoteDetails MeterRideQuoteDetails {..} = do
  pure DQuote.MeterRideQuoteDetails {..}

mkOneWayQuoteDetails :: DistanceUnit -> OneWayQuoteDetails -> DQuote.OneWayQuoteDetails
mkOneWayQuoteDetails distanceUnit OneWayQuoteDetails {..} =
  DQuote.OneWayQuoteDetails {distanceToNearestDriver = convertHighPrecMetersToDistance distanceUnit distanceToNearestDriver, ..}

buildOneWaySpecialZoneQuoteDetails :: MonadFlow m => OneWaySpecialZoneQuoteDetails -> m DSpecialZoneQuote.SpecialZoneQuote
buildOneWaySpecialZoneQuoteDetails OneWaySpecialZoneQuoteDetails {..} = do
  id <- generateGUID
  now <- getCurrentTime
  let createdAt = now
      updatedAt = now
  pure DSpecialZoneQuote.SpecialZoneQuote {..}

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

-- getEstimateTags :: SEST.EstimateTagsData ->  Flow (Maybe [Text])
getTaggedEstimate ::
  Maybe Double ->
  Maybe Double ->
  Maybe HighPrecMoney ->
  Maybe HighPrecMoney ->
  Maybe SEST.UserType ->
  UTCTime ->
  Id DMerchantOperatingCity.MerchantOperatingCity ->
  DEstimate.Estimate ->
  Flow DEstimate.Estimate
getTaggedEstimate autoQar cabQar autoPrice nonACPrice userType localTime mocId estimate = do
  let logicInput =
        SEST.EstimateTagsData
          { autoQAR = autoQar,
            cabQAR = cabQar,
            autoPrice = autoPrice,
            cabPrice = nonACPrice,
            userType = userType,
            vehicleCategory = estimate.vehicleCategory,
            vehicleServiceTierType = estimate.vehicleServiceTierType
          }
  (allLogics, _mbVersion) <- DynamicLogic.getAppDynamicLogic (cast mocId) LYT.ESTIMATE_TAGS localTime Nothing Nothing
  response <- withTryCatch "runLogics:EstimateTags" $ LYDL.runLogicsWithDebugLog (cast mocId) LYT.ESTIMATE_TAGS allLogics logicInput
  res <- case response of
    Left e -> do
      logError $ "Error in running EstimateTagsLogics - " <> show e <> " - " <> show logicInput <> " - " <> show allLogics
      return (Just [])
    Right resp -> do
      case (A.fromJSON resp.result :: Result SEST.EstimateTagsResult) of
        A.Success result -> do
          logTagInfo ("estimateTags-" <> getId estimate.id) ("result.tags: " <> show result.tags)
          return (Just result.tags)
        A.Error e -> do
          logError $ "Error in parsing EstimateTagsResult - " <> show e <> " - " <> show resp.result <> " - " <> show logicInput <> " - " <> show allLogics
          return Nothing
  return $ estimate{estimateTags = res}

getAutoPrice :: [DEstimate.Estimate] -> Flow (Maybe HighPrecMoney)
getAutoPrice estimates = do
  let autoPrice = filter (\estimate -> estimate.vehicleServiceTierType == DVST.AUTO_RICKSHAW) listToMaybe estimates
  pure $ autoPrice <&> (.totalFareRange.maxFare.amount)

getNonACPrice :: [DEstimate.Estimate] -> Flow (Maybe HighPrecMoney)
getNonACPrice estimates = do
  let nonACPrice = filter (\estimate -> estimate.vehicleServiceTierType == DVST.TAXI) listToMaybe estimates
  pure $ nonACPrice <&> (.totalFareRange.maxFare.amount)

getAutoQar :: [DEstimate.Estimate] -> Flow (Maybe Double)
getAutoQar estimates = do
  let autoQar = filter (\estimate -> estimate.vehicleServiceTierType == DVST.AUTO_RICKSHAW) listToMaybe estimates
  pure $ autoQar >>= (.qar)

getCabQar :: [DEstimate.Estimate] -> Flow (Maybe Double)
getCabQar estimates = do
  let cabQar = filter (\estimate -> estimate.vehicleServiceTierType == DVST.TAXI) listToMaybe estimates
  pure $ cabQar >>= (.qar)

personVehicleCategory :: Person.Person -> Maybe Enums.VehicleCategory
personVehicleCategory person = SLS.mostFrequent person.lastUsedVehicleCategories
