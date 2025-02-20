module Domain.Action.UI.DriverOnboardingV2 where

import qualified API.Types.UI.DriverOnboardingV2
import qualified API.Types.UI.DriverOnboardingV2 as APITypes
import qualified AWS.S3 as S3
import qualified Control.Monad.Extra as CME
import Data.Maybe
import qualified Data.Text as T
import Data.Time (defaultTimeLocale, formatTime)
import qualified Data.Time as DT
import qualified Domain.Action.UI.DriverOnboarding.Image as Image
import qualified Domain.Types.AadhaarCard
import Domain.Types.BackgroundVerification
import Domain.Types.Common
import qualified Domain.Types.DocumentVerificationConfig
import qualified Domain.Types.DocumentVerificationConfig as DTO
import qualified Domain.Types.DriverBankAccount as DDBA
import qualified Domain.Types.DriverPanCard as Domain
import Domain.Types.DriverSSN
import Domain.Types.FarePolicy
import qualified Domain.Types.HyperVergeSdkLogs as DomainHVSdkLogs
import qualified Domain.Types.Image as Image
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import Domain.Types.TransporterConfig
import qualified Domain.Types.VehicleCategory as DVC
import Domain.Types.VehicleServiceTier
import Environment
import EulerHS.Prelude hiding (id)
import qualified EulerHS.Prelude
import Kernel.Beam.Functions
import qualified Kernel.External.BackgroundVerification.Interface as BackgroundVerification
import Kernel.External.Encryption
import qualified Kernel.External.Payment.Interface as Payment
import Kernel.External.Types (Language (..), ServiceFlow)
import qualified Kernel.External.Verification.Interface as VI
import qualified Kernel.Prelude
import Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Beckn.DecimalValue as DecimalValue
import qualified Kernel.Types.Documents as Documents
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.DriverOnboarding
import SharedLogic.FareCalculator
import SharedLogic.FarePolicy
import qualified SharedLogic.Merchant as SMerchant
import SharedLogic.VehicleServiceTier
import qualified Storage.Cac.TransporterConfig as CQTC
import qualified Storage.CachedQueries.DocumentVerificationConfig as CQDVC
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.VehicleServiceTier as CQVST
import qualified Storage.Queries.AadhaarCard as QAadhaarCard
import qualified Storage.Queries.BackgroundVerification as QBV
import qualified Storage.Queries.DriverBankAccount as QDBA
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.DriverLicense as QDL
import qualified Storage.Queries.DriverPanCard as QDPC
import qualified Storage.Queries.DriverSSN as QDriverSSN
import qualified Storage.Queries.HyperVergeSdkLogs as HVSdkLogsQuery
import qualified Storage.Queries.Image as ImageQuery
import qualified Storage.Queries.Person as PersonQuery
import qualified Storage.Queries.Translations as MTQuery
import qualified Storage.Queries.Vehicle as QVehicle
import qualified Tools.BackgroundVerification as BackgroundVerificationT
import Tools.Error
import qualified Tools.Payment as TPayment
import qualified Tools.Verification as Verification
import Utils.Common.Cac.KeyNameConstants

stringToPrice :: Currency -> Text -> Maybe Price
stringToPrice currency value = do
  (DecimalValue.DecimalValue v) <- DecimalValue.valueFromString value
  return $
    Price
      { amountInt = Money $ Kernel.Prelude.roundToIntegral v,
        amount = HighPrecMoney v,
        currency
      }

mkDocumentVerificationConfigAPIEntity :: Language -> Domain.Types.DocumentVerificationConfig.DocumentVerificationConfig -> Environment.Flow API.Types.UI.DriverOnboardingV2.DocumentVerificationConfigAPIEntity
mkDocumentVerificationConfigAPIEntity language Domain.Types.DocumentVerificationConfig.DocumentVerificationConfig {..} = do
  mbTitle <- MTQuery.findByErrorAndLanguage (show documentType <> "_Title") language
  mbDescription <- MTQuery.findByErrorAndLanguage (show documentType <> "_Description") language
  return $
    API.Types.UI.DriverOnboardingV2.DocumentVerificationConfigAPIEntity
      { title = maybe title (.message) mbTitle,
        description = maybe description (Just . (.message)) mbDescription,
        ..
      }

getOnboardingConfigs ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Prelude.Maybe Kernel.Prelude.Bool ->
    Kernel.Prelude.Maybe Kernel.Prelude.Bool ->
    Environment.Flow API.Types.UI.DriverOnboardingV2.DocumentVerificationConfigList
  )
getOnboardingConfigs (mbPersonId, _, merchantOpCityId) makeSelfieAadhaarPanMandatory mbOnlyVehicle = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  person <- runInReplica $ PersonQuery.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  let personLangauge = fromMaybe ENGLISH person.language
  cabConfigsRaw <- CQDVC.findByMerchantOpCityIdAndCategory merchantOpCityId DVC.CAR
  autoConfigsRaw <- CQDVC.findByMerchantOpCityIdAndCategory merchantOpCityId DVC.AUTO_CATEGORY
  bikeConfigsRaw <- CQDVC.findByMerchantOpCityIdAndCategory merchantOpCityId DVC.MOTORCYCLE
  ambulanceConfigsRaw <- CQDVC.findByMerchantOpCityIdAndCategory merchantOpCityId DVC.AMBULANCE
  truckConfigsRaw <- CQDVC.findByMerchantOpCityIdAndCategory merchantOpCityId DVC.TRUCK
  busConfigsRaw <- CQDVC.findByMerchantOpCityIdAndCategory merchantOpCityId DVC.BUS
  cabConfigs <- filterInCompatibleFlows <$> mapM (mkDocumentVerificationConfigAPIEntity personLangauge) (filterVehicleDocuments cabConfigsRaw)
  autoConfigs <- filterInCompatibleFlows <$> mapM (mkDocumentVerificationConfigAPIEntity personLangauge) (filterVehicleDocuments autoConfigsRaw)
  bikeConfigs <- filterInCompatibleFlows <$> mapM (mkDocumentVerificationConfigAPIEntity personLangauge) (filterVehicleDocuments bikeConfigsRaw)
  ambulanceConfigs <- filterInCompatibleFlows <$> mapM (mkDocumentVerificationConfigAPIEntity personLangauge) (filterVehicleDocuments ambulanceConfigsRaw)
  truckConfigs <- filterInCompatibleFlows <$> mapM (mkDocumentVerificationConfigAPIEntity personLangauge) (filterVehicleDocuments truckConfigsRaw)
  busConfigs <- filterInCompatibleFlows <$> mapM (mkDocumentVerificationConfigAPIEntity personLangauge) (filterVehicleDocuments busConfigsRaw)
  return $
    API.Types.UI.DriverOnboardingV2.DocumentVerificationConfigList
      { cabs = toMaybe cabConfigs,
        autos = toMaybe autoConfigs,
        bikes = toMaybe bikeConfigs,
        ambulances = toMaybe ambulanceConfigs,
        trucks = toMaybe truckConfigs,
        bus = toMaybe busConfigs
      }
  where
    toMaybe :: [a] -> Kernel.Prelude.Maybe [a]
    toMaybe [] = Kernel.Prelude.Nothing
    toMaybe xs = Kernel.Prelude.Just xs

    filterVehicleDocuments :: [Domain.Types.DocumentVerificationConfig.DocumentVerificationConfig] -> [Domain.Types.DocumentVerificationConfig.DocumentVerificationConfig]
    filterVehicleDocuments docs =
      if mbOnlyVehicle == Just True
        then filter (\Domain.Types.DocumentVerificationConfig.DocumentVerificationConfig {..} -> documentType `elem` vehicleDocumentTypes) docs
        else docs
    filterInCompatibleFlows :: [API.Types.UI.DriverOnboardingV2.DocumentVerificationConfigAPIEntity] -> [API.Types.UI.DriverOnboardingV2.DocumentVerificationConfigAPIEntity]
    filterInCompatibleFlows = filter (\doc -> not (fromMaybe False doc.filterForOldApks) || fromMaybe False makeSelfieAadhaarPanMandatory)

getDriverRateCard ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Prelude.Maybe Meters ->
    Kernel.Prelude.Maybe Minutes ->
    Kernel.Prelude.Maybe Domain.Types.Common.ServiceTierType ->
    Environment.Flow [API.Types.UI.DriverOnboardingV2.RateCardResp]
  )
getDriverRateCard (mbPersonId, _, merchantOperatingCityId) reqDistance reqDuration mbServiceTierType = do
  distanceUnit <- SMerchant.getDistanceUnitByMerchantOpCity merchantOperatingCityId
  let mbDistance = reqDistance
      mbDuration = reqDuration
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  transporterConfig <- CQTC.findByMerchantOpCityId merchantOperatingCityId (Just (DriverId (cast personId)))
  driverInfo <- runInReplica $ QDI.findById personId >>= fromMaybeM DriverInfoNotFound
  vehicle <- runInReplica $ QVehicle.findById personId >>= fromMaybeM (VehicleNotFound personId.getId)
  -- driverStats <- runInReplica $ QDriverStats.findById personId >>= fromMaybeM DriverInfoNotFound
  cityVehicleServiceTiers <- CQVST.findAllByMerchantOpCityId merchantOperatingCityId
  let driverVehicleServiceTierTypes = (\(vehicleServiceTier, _) -> vehicleServiceTier.serviceTierType) <$> selectVehicleTierForDriverWithUsageRestriction False driverInfo vehicle cityVehicleServiceTiers
  case mbServiceTierType of
    Just serviceTierType -> do
      when (serviceTierType `notElem` driverVehicleServiceTierTypes) $ throwError $ InvalidRequest ("Service tier " <> show serviceTierType <> " not available for driver")
      mbRateCard <- getRateCardForServiceTier mbDistance mbDuration transporterConfig (OneWay OneWayOnDemandDynamicOffer) distanceUnit serviceTierType
      return $ maybeToList mbRateCard
    Nothing -> do
      rateCards <- mapM (getRateCardForServiceTier mbDistance mbDuration transporterConfig (OneWay OneWayOnDemandDynamicOffer) distanceUnit) driverVehicleServiceTierTypes
      return $ catMaybes rateCards
  where
    mkBreakupItem :: Text -> Text -> Maybe API.Types.UI.DriverOnboardingV2.RateCardItem
    mkBreakupItem title valueInText = do
      priceObject <- stringToPrice INR valueInText -- change INR to proper currency after Roman changes
      return $
        API.Types.UI.DriverOnboardingV2.RateCardItem
          { title,
            price = priceObject.amountInt,
            priceWithCurrency = mkPriceAPIEntity priceObject
          }

    getRateCardForServiceTier :: Maybe Meters -> Maybe Minutes -> Maybe TransporterConfig -> TripCategory -> DistanceUnit -> Domain.Types.Common.ServiceTierType -> Environment.Flow (Maybe API.Types.UI.DriverOnboardingV2.RateCardResp)
    getRateCardForServiceTier mbDistance mbDuration transporterConfig tripCategory distanceUnit serviceTierType = do
      now <- getCurrentTime
      eitherFullFarePolicy <-
        try @_ @SomeException (getFarePolicy Nothing Nothing Nothing Nothing Nothing merchantOperatingCityId False (OneWay OneWayOnDemandDynamicOffer) serviceTierType Nothing Nothing Nothing Nothing)
          >>= \case
            Left _ -> try @_ @SomeException $ getFarePolicy Nothing Nothing Nothing Nothing Nothing merchantOperatingCityId False (Delivery OneWayOnDemandDynamicOffer) serviceTierType Nothing Nothing Nothing Nothing
            Right farePolicy -> return $ Right farePolicy
      case eitherFullFarePolicy of
        Left _ -> return Nothing
        Right fullFarePolicy -> do
          let isPeak =
                fromMaybe False $
                  fullFarePolicy.congestionChargeMultiplier <&> \case
                    BaseFareAndExtraDistanceFare congestionChargeMultiplier -> congestionChargeMultiplier > 1
                    ExtraDistanceFare congestionChargeMultiplier -> congestionChargeMultiplier > 1
          let mbIsNight =
                if isRentalTrip tripCategory
                  then Just $ isNightAllowanceApplicable fullFarePolicy.nightShiftBounds now now (maybe 19800 (.timeDiffFromUtc) transporterConfig)
                  else isNightShift <$> fullFarePolicy.nightShiftBounds <*> Just now
          let isNight = fromMaybe False mbIsNight
          fareParams <-
            calculateFareParameters
              CalculateFareParametersParams
                { farePolicy = fullFarePolicy,
                  actualDistance = mbDistance,
                  rideTime = now,
                  waitingTime = Nothing,
                  stopWaitingTimes = [],
                  returnTime = Nothing,
                  vehicleAge = Nothing,
                  estimatedCongestionCharge = Nothing,
                  roundTrip = False,
                  noOfStops = 0,
                  actualRideDuration = Nothing,
                  avgSpeedOfVehicle = Nothing,
                  driverSelectedFare = Nothing,
                  customerExtraFee = Nothing,
                  nightShiftCharge = Nothing,
                  customerCancellationDues = Nothing,
                  nightShiftOverlapChecking = isFixedNightCharge tripCategory,
                  estimatedDistance = mbDistance,
                  estimatedRideDuration = minutesToSeconds <$> mbDuration,
                  timeDiffFromUtc = transporterConfig <&> (.timeDiffFromUtc),
                  currency = INR, -- fix it later
                  distanceUnit,
                  tollCharges = Nothing,
                  merchantOperatingCityId = Just merchantOperatingCityId
                }
          let totalFareAmount = perRideKmFareParamsSum fareParams
              perKmAmount :: Rational = totalFareAmount.getHighPrecMoney / fromIntegral (maybe 1 (getKilometers . metersToKilometers) mbDistance)
              perKmRate =
                PriceAPIEntity
                  { amount = HighPrecMoney perKmAmount,
                    currency = INR
                  }
              totalFare =
                PriceAPIEntity
                  { amount = totalFareAmount,
                    currency = INR
                  }
              perMinuteRate = getPerMinuteRate fareParams
          let rateCardItems = catMaybes $ mkFarePolicyBreakups EulerHS.Prelude.id mkBreakupItem Nothing Nothing totalFare.amount Nothing (fullFarePolicyToFarePolicy fullFarePolicy)
          return $
            Just $
              API.Types.UI.DriverOnboardingV2.RateCardResp
                { serviceTierType,
                  perKmRate,
                  totalFare,
                  perMinuteRate,
                  tripCategory,
                  farePolicyHour = if isPeak then APITypes.Peak else if isNight then APITypes.Night else APITypes.NonPeak,
                  rateCardItems
                }

postDriverUpdateAirCondition ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    API.Types.UI.DriverOnboardingV2.UpdateAirConditionUpdateRequest ->
    Environment.Flow Kernel.Types.APISuccess.APISuccess
  )
postDriverUpdateAirCondition (mbPersonId, _, merchantOperatingCityId) API.Types.UI.DriverOnboardingV2.UpdateAirConditionUpdateRequest {..} = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  cityVehicleServiceTiers <- CQVST.findAllByMerchantOpCityId merchantOperatingCityId
  checkAndUpdateAirConditioned False isAirConditioned personId cityVehicleServiceTiers Nothing
  now <- getCurrentTime
  QDI.updateLastACStatusCheckedAt (Just now) personId
  return Success

getDriverVehicleServiceTiers ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Environment.Flow API.Types.UI.DriverOnboardingV2.DriverVehicleServiceTiers
  )
getDriverVehicleServiceTiers (mbPersonId, _, merchantOpCityId) = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  person <- runInReplica $ PersonQuery.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  driverInfo <- runInReplica $ QDI.findById personId >>= fromMaybeM DriverInfoNotFound
  vehicle <- runInReplica $ QVehicle.findById personId >>= fromMaybeM (VehicleDoesNotExist personId.getId)
  -- driverStats <- runInReplica $ QDriverStats.findById personId >>= fromMaybeM DriverInfoNotFound
  cityVehicleServiceTiers <- CQVST.findAllByMerchantOpCityId merchantOpCityId
  let personLangauge = fromMaybe ENGLISH person.language

  let driverVehicleServiceTierTypes = selectVehicleTierForDriverWithUsageRestriction False driverInfo vehicle cityVehicleServiceTiers
  let serviceTierACThresholds = map (\(VehicleServiceTier {..}, _) -> airConditionedThreshold) driverVehicleServiceTierTypes
  let isACCheckEnabledForCity = any isJust serviceTierACThresholds
  let isACAllowedForDriver = checkIfACAllowedForDriver driverInfo (catMaybes serviceTierACThresholds)
  let isACWorkingForVehicle = vehicle.airConditioned /= Just False
  let isACWorking = isACAllowedForDriver && isACWorkingForVehicle
  let tierOptions =
        driverVehicleServiceTierTypes <&> \(VehicleServiceTier {..}, usageRestricted) -> do
          let isNonACDefault = isACCheckEnabledForCity && not isACWorking && isNothing airConditionedThreshold
              isNonOxygenDefault = oxygen /= Just 1
          API.Types.UI.DriverOnboardingV2.DriverVehicleServiceTier
            { isSelected = (serviceTierType `elem` vehicle.selectedServiceTiers) || (isNonACDefault && isNonOxygenDefault),
              isDefault = (vehicleCategory /= Just DVC.AMBULANCE) && ((vehicle.variant `elem` defaultForVehicleVariant) || isNonACDefault), -- No default in Ambulance
              isUsageRestricted = Just usageRestricted,
              priority = Just priority,
              airConditioned = airConditionedThreshold,
              ..
            }

  mbAirConditioned <-
    if isACCheckEnabledForCity
      then do
        restrictionMessageItem <-
          if not isACAllowedForDriver
            then MTQuery.findByErrorAndLanguage "AC_RESTRICTION_MESSAGE" personLangauge
            else
              if isACWorkingForVehicle
                then MTQuery.findByErrorAndLanguage "AC_WORKING_MESSAGE" personLangauge
                else return Nothing
        return $
          Just $
            API.Types.UI.DriverOnboardingV2.AirConditionedTier
              { isWorking = isACWorking,
                restrictionMessage = restrictionMessageItem <&> (.message),
                usageRestrictionType = driverInfo.acUsageRestrictionType
              }
      else return Nothing

  {- Atleast one intercity/rental non usage restricted service tier should be selected for displaying intercity/rental rides toggle option to driver -}
  let canSwitchToInterCity' =
        foldl
          ( \canSwitchToInterCityForAnyServiceTierSoFar (selectedServiceTier, isUsageRestricted) ->
              if isUsageRestricted
                then canSwitchToInterCityForAnyServiceTierSoFar
                else canSwitchToInterCityForAnyServiceTierSoFar || fromMaybe True selectedServiceTier.isIntercityEnabled
          )
          False
          driverVehicleServiceTierTypes
      canSwitchToRental' =
        foldl
          ( \canSwitchToRentalsForAnyServiceTierSoFar (selectedServiceTier, isUsageRestricted) ->
              if isUsageRestricted
                then canSwitchToRentalsForAnyServiceTierSoFar
                else canSwitchToRentalsForAnyServiceTierSoFar || fromMaybe True selectedServiceTier.isRentalsEnabled
          )
          False
          driverVehicleServiceTierTypes
      canSwitchToIntraCity' = (any (\(st, _) -> st.vehicleCategory == Just DVC.CAR) driverVehicleServiceTierTypes) && (canSwitchToInterCity' || canSwitchToRental')

  return $
    API.Types.UI.DriverOnboardingV2.DriverVehicleServiceTiers
      { tiers = tierOptions,
        canSwitchToRental = if canSwitchToRental' then Just driverInfo.canSwitchToRental else Nothing,
        canSwitchToInterCity = if canSwitchToInterCity' then Just driverInfo.canSwitchToInterCity else Nothing,
        canSwitchToIntraCity = if canSwitchToIntraCity' then Just driverInfo.canSwitchToIntraCity else Nothing,
        airConditioned = mbAirConditioned
      }

postDriverUpdateServiceTiers ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    API.Types.UI.DriverOnboardingV2.DriverVehicleServiceTiers ->
    Environment.Flow APISuccess
  )
postDriverUpdateServiceTiers (mbPersonId, _, merchantOperatingCityId) API.Types.UI.DriverOnboardingV2.DriverVehicleServiceTiers {..} = do
  -- Todo: Handle oxygen,ventilator here also. For now, frontend can handle
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  -- driverStats <- runInReplica $ QDriverStats.findById personId >>= fromMaybeM DriverInfoNotFound
  cityVehicleServiceTiers <- CQVST.findAllByMerchantOpCityId merchantOperatingCityId

  whenJust airConditioned $ \ac -> checkAndUpdateAirConditioned False ac.isWorking personId cityVehicleServiceTiers Nothing
  driverInfo <- QDI.findById personId >>= fromMaybeM DriverInfoNotFound
  vehicle <- QVehicle.findById personId >>= fromMaybeM (VehicleNotFound personId.getId)

  let driverVehicleServiceTierTypes = selectVehicleTierForDriverWithUsageRestriction False driverInfo vehicle cityVehicleServiceTiers
  mbSelectedServiceTiers <-
    driverVehicleServiceTierTypes `forM` \(driverServiceTier, _) -> do
      let isAlreadySelected = driverServiceTier.serviceTierType `elem` vehicle.selectedServiceTiers
          isDefault = vehicle.variant `elem` driverServiceTier.defaultForVehicleVariant
          mbServiceTierDriverRequest = find (\tier -> tier.serviceTierType == driverServiceTier.serviceTierType) tiers
          isSelected = maybe isAlreadySelected (.isSelected) mbServiceTierDriverRequest

      if isSelected || (isDefault && (vehicle.category /= Just DVC.AMBULANCE)) -- Suppressing isDefault check for Ambulance
        then return $ Just driverServiceTier
        else return Nothing
  let selectedServiceTierTypes = map (.serviceTierType) $ catMaybes mbSelectedServiceTiers
  QVehicle.updateSelectedServiceTiers selectedServiceTierTypes personId

  {- Atleast one intercity/rental non usage resctricted service tier should be selected for getting intercity/rental rides -}
  let canSwitchToInterCity' =
        foldl
          ( \canSwitchToInterCityForAnyServiceTierSoFar (selectedServiceTier, isUsageRestricted) ->
              if isUsageRestricted
                then canSwitchToInterCityForAnyServiceTierSoFar
                else canSwitchToInterCityForAnyServiceTierSoFar || (fromMaybe driverInfo.canSwitchToInterCity canSwitchToInterCity && fromMaybe True selectedServiceTier.isIntercityEnabled)
          )
          False
          driverVehicleServiceTierTypes
      canSwitchToRental' =
        foldl
          ( \canSwitchToRentalsForAnyServiceTierSoFar (selectedServiceTier, isUsageRestricted) ->
              if isUsageRestricted
                then canSwitchToRentalsForAnyServiceTierSoFar
                else canSwitchToRentalsForAnyServiceTierSoFar || (fromMaybe driverInfo.canSwitchToRental canSwitchToRental && fromMaybe True selectedServiceTier.isRentalsEnabled)
          )
          False
          driverVehicleServiceTierTypes

      canSwitchToIntraCity' =
        if (any (\(st, _) -> st.vehicleCategory == Just DVC.CAR) driverVehicleServiceTierTypes) && (canSwitchToInterCity' || canSwitchToRental')
          then fromMaybe driverInfo.canSwitchToIntraCity canSwitchToIntraCity
          else True

  QDI.updateRentalInterCityAndIntraCitySwitch canSwitchToRental' canSwitchToInterCity' canSwitchToIntraCity' personId

  return Success

postDriverRegisterSsn ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    API.Types.UI.DriverOnboardingV2.SSNReq ->
    Environment.Flow APISuccess
  )
postDriverRegisterSsn (mbPersonId, _, _) API.Types.UI.DriverOnboardingV2.SSNReq {..} = do
  driverId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  ssnEnc <- encrypt ssn
  mbSsnEntry <- QDriverSSN.findBySSN (ssnEnc & hash)
  whenJust mbSsnEntry $ \ssnEntry -> do
    unless (ssnEntry.driverId == driverId) $ throwError (InvalidRequest "SSN Already linked to another Driver.")
  ssnEntry <- buildDriverSSN ssnEnc driverId
  QDriverSSN.upsert ssnEntry
  return Success
  where
    buildDriverSSN ssnEnc driverId = do
      id <- generateGUID
      return $
        Domain.Types.DriverSSN.DriverSSN
          { id,
            driverId,
            ssn = ssnEnc,
            verificationStatus = Documents.MANUAL_VERIFICATION_REQUIRED,
            rejectReason = Nothing
          }

postDriverBackgroundVerification ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Environment.Flow Kernel.Types.APISuccess.APISuccess
  )
postDriverBackgroundVerification (mbPersonId, merchantId, merchantOpCityId) = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  person <- runInReplica $ PersonQuery.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  driverInfo <- runInReplica $ QDI.findById personId >>= fromMaybeM DriverInfoNotFound
  merchantOpCity <- CQMOC.findById person.merchantOperatingCityId >>= fromMaybeM (MerchantOperatingCityNotFound person.merchantOperatingCityId.getId)
  mbBackgroundVerification <- QBV.findByDriverId personId
  candidateId <-
    case mbBackgroundVerification of
      Just backgroundVerification -> return backgroundVerification.candidateId
      Nothing -> do
        candidate <- BackgroundVerificationT.createCandidate merchantId merchantOpCityId =<< buildCreateCandidateReq person driverInfo merchantOpCity
        return candidate.id
  invitation <- BackgroundVerificationT.createInvitation merchantId merchantOpCityId $ buildCreateInvitationReq merchantOpCity candidateId
  QBV.upsert =<< buildBackgroundVerification personId candidateId invitation
  return Success
  where
    buildBackgroundVerification personId candidateId invitation = do
      now <- getCurrentTime
      return $
        BackgroundVerification
          { candidateId = candidateId,
            driverId = personId,
            invitationId = invitation.id,
            invitationUrl = invitation.invitationUrl,
            reportId = Nothing,
            invitationStatus = Documents.PENDING,
            reportStatus = Documents.PENDING,
            expiresAt = invitation.expiresAt,
            merchantId = merchantId,
            merchantOperatingCityId = merchantOpCityId,
            createdAt = now,
            updatedAt = now
          }

    buildCreateCandidateReq person driverInfo merchantOpCity = do
      email <- person.email & fromMaybeM (DriverEmailNotFound person.id.getId)
      mobileNumber <- mapM decrypt person.mobileNumber
      return $
        BackgroundVerification.CreateCandidateReq
          { email = email,
            ssn = Nothing,
            firstName = person.firstName,
            middleName = Nothing,
            lastName = person.lastName,
            phone = mobileNumber,
            dob = driverInfo.driverDob,
            workLocationCountry = merchantOpCity.country,
            workLocationState = merchantOpCity.state,
            workLocationCity = merchantOpCity.city,
            driverLicenseNumber = Nothing,
            driverLicenseState = Nothing,
            zipCode = Nothing
          }

    buildCreateInvitationReq merchantOpCity candidateId =
      BackgroundVerification.CreateInvitationReqI
        { candidateId = candidateId,
          workLocationCountry = merchantOpCity.country,
          workLocationState = merchantOpCity.state,
          workLocationCity = merchantOpCity.city
        }

postDriverRegisterPancard ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    API.Types.UI.DriverOnboardingV2.DriverPanReq ->
    Environment.Flow Kernel.Types.APISuccess.APISuccess
  )
postDriverRegisterPancard (mbPersonId, merchantId, merchantOpCityId) req = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  person <- runInReplica $ PersonQuery.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  when (isJust req.validationStatus) $ checkIfGenuineReq req
  getImage req.imageId1 personId ------- Just checking whether the image exists or not
  let verificationStatus = maybe Documents.PENDING Image.convertValidationStatusToVerificationStatus req.validationStatus
  mbPanInfo <- QDPC.findUnInvalidByPanNumber req.panNumber
  whenJust mbPanInfo $ \panInfo -> do
    when (panInfo.driverId /= personId) $ do
      ImageQuery.deleteById req.imageId1
      throwError $ DocumentAlreadyLinkedToAnotherDriver "PAN"
    when (panInfo.verificationStatus == Documents.MANUAL_VERIFICATION_REQUIRED) $ do
      ImageQuery.deleteById req.imageId1
      throwError $ DocumentUnderManualReview "PAN"
    when (panInfo.verificationStatus == Documents.VALID) $ do
      ImageQuery.deleteById req.imageId1
      throwError $ DocumentAlreadyValidated "PAN"
  QDPC.upsertPanRecord =<< buildPanCard merchantId person req verificationStatus (Just merchantOpCityId)
  return Success
  where
    getImage :: Kernel.Types.Id.Id Image.Image -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Environment.Flow ()
    getImage imageId personId = do
      imageMetadata <- ImageQuery.findById imageId >>= fromMaybeM (ImageNotFound imageId.getId)
      unless (imageMetadata.personId == personId) $ throwError (ImageNotFound imageId.getId)
      unless (imageMetadata.imageType == DTO.PanCard) $
        throwError (ImageInvalidType (show DTO.PanCard) (show imageMetadata.imageType))

    checkIfGenuineReq :: (ServiceFlow m r) => API.Types.UI.DriverOnboardingV2.DriverPanReq -> m ()
    checkIfGenuineReq API.Types.UI.DriverOnboardingV2.DriverPanReq {..} = do
      (txnId, valStatus) <- CME.fromMaybeM (Image.throwValidationError (Just imageId1) Nothing (Just "Cannot find necessary data for SDK response!!!!")) (return $ (,) <$> transactionId <*> validationStatus)
      hvResp <- Verification.verifySdkResp merchantId merchantOpCityId (VI.VerifySdkDataReq txnId)
      (respTxnId, respStatus, respUserDetails) <- CME.fromMaybeM (Image.throwValidationError (Just imageId1) Nothing (Just "Invalid data recieved while validating data.")) (return $ (,,) <$> hvResp.transactionId <*> hvResp.status <*> hvResp.userDetails)
      when (respTxnId /= txnId) $ void $ Image.throwValidationError (Just imageId1) Nothing Nothing
      when (Image.convertHVStatusToValidationStatus respStatus /= valStatus) $ void $ Image.throwValidationError (Just imageId1) Nothing Nothing
      case respUserDetails of
        VI.HVPanFlow (VI.PanFlow {..}) -> do
          panNum <- CME.fromMaybeM (Image.throwValidationError (Just imageId1) Nothing (Just "PAN number not found in SDK validation response even though it's compulsory for Pan")) (return pan)
          when (panNumber /= panNum) $ void $ Image.throwValidationError (Just imageId1) Nothing Nothing
          when (nameOnCard /= name) $ void $ Image.throwValidationError (Just imageId1) Nothing Nothing
          when (isJust dateOfBirth && (formatUTCToDateString <$> dateOfBirth) /= (T.unpack <$> dob)) $ do
            logDebug $ "date of Birth and dob is : " <> show (formatUTCToDateString <$> dateOfBirth) <> " " <> show dob
            void $ Image.throwValidationError (Just imageId1) Nothing Nothing
        _ -> void $ Image.throwValidationError (Just imageId1) Nothing Nothing
      where
        formatUTCToDateString :: UTCTime -> String
        formatUTCToDateString utcTime = formatTime defaultTimeLocale "%d-%m-%Y" utcTime

buildPanCard ::
  Kernel.Types.Id.Id Domain.Types.Merchant.Merchant ->
  Domain.Types.Person.Person ->
  API.Types.UI.DriverOnboardingV2.DriverPanReq ->
  Documents.VerificationStatus ->
  Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity) ->
  Environment.Flow Domain.DriverPanCard
buildPanCard merchantId person API.Types.UI.DriverOnboardingV2.DriverPanReq {..} verificationStatus merchantOperatingCityId = do
  now <- getCurrentTime
  id <- generateGUID
  encryptedPan <- encrypt panNumber
  return
    Domain.DriverPanCard
      { consentTimestamp = fromMaybe now consentTimestamp,
        documentImageId1 = imageId1,
        documentImageId2 = imageId2,
        driverDob = dateOfBirth,
        driverId = person.id,
        driverName = Just $ fromMaybe person.firstName nameOnCard,
        failedRules = [],
        id = id,
        panCardNumber = encryptedPan,
        merchantId = Just merchantId,
        createdAt = now,
        updatedAt = now,
        verificationStatus = verificationStatus,
        driverNameOnGovtDB = nameOnGovtDB,
        ..
      }

getDriverRegisterGetLiveSelfie ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Image.SelfieFetchStatus ->
    Environment.Flow API.Types.UI.DriverOnboardingV2.GetLiveSelfieResp
  )
getDriverRegisterGetLiveSelfie (mbPersonId, _, _) requiredStatus = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  imageEntity <- ImageQuery.findByPersonIdImageTypeAndValidationStatus personId DTO.ProfilePhoto requiredStatus >>= fromMaybeM (ImageNotFound $ "Selfie image with requiredStatus = " <> show requiredStatus <> " for personId = " <> show personId)
  image <- S3.get $ T.unpack imageEntity.s3Path
  return $ API.Types.UI.DriverOnboardingV2.GetLiveSelfieResp image

postDriverRegisterAadhaarCard ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    API.Types.UI.DriverOnboardingV2.AadhaarCardReq ->
    Environment.Flow APISuccess
  )
postDriverRegisterAadhaarCard (mbPersonId, merchantId, merchantOperatingCityId) req@API.Types.UI.DriverOnboardingV2.AadhaarCardReq {..} = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  prevTry <- QAadhaarCard.findByPrimaryKey personId
  checkIfGenuineReq req
  whenJust prevTry $ \aadhaarEntity -> do
    when (aadhaarEntity.verificationStatus == Documents.MANUAL_VERIFICATION_REQUIRED) $
      throwError $ DocumentUnderManualReview "Aadhaar"
    when (aadhaarEntity.verificationStatus == Documents.VALID) $
      throwError $ DocumentAlreadyValidated "Aadhaar"
  QAadhaarCard.upsertAadhaarRecord =<< makeAadhaarCardEntity personId
  return Success
  where
    makeAadhaarCardEntity personId = do
      currTime <- getCurrentTime
      let verificationStatus = Image.convertValidationStatusToVerificationStatus validationStatus
      return $
        Domain.Types.AadhaarCard.AadhaarCard
          { driverId = personId,
            createdAt = currTime,
            updatedAt = currTime,
            aadhaarNumberHash = Nothing,
            driverGender = Nothing,
            driverImage = Nothing,
            driverImagePath = Nothing,
            ..
          }
    checkIfGenuineReq :: ServiceFlow m r => API.Types.UI.DriverOnboardingV2.AadhaarCardReq -> m ()
    checkIfGenuineReq aadhaarReq = do
      hvResp <- Verification.verifySdkResp merchantId merchantOperatingCityId (VI.VerifySdkDataReq aadhaarReq.transactionId)
      (respTxnId, respStatus, respUserDetails) <- CME.fromMaybeM (Image.throwValidationError aadhaarReq.aadhaarBackImageId aadhaarReq.aadhaarFrontImageId (Just "Invalid data recieved while validating data.")) (return $ (,,) <$> hvResp.transactionId <*> hvResp.status <*> hvResp.userDetails)
      when (respTxnId /= aadhaarReq.transactionId) $ void $ Image.throwValidationError aadhaarReq.aadhaarBackImageId aadhaarReq.aadhaarFrontImageId Nothing
      when (Image.convertHVStatusToValidationStatus respStatus /= aadhaarReq.validationStatus) $ void $ Image.throwValidationError aadhaarReq.aadhaarBackImageId aadhaarReq.aadhaarFrontImageId Nothing
      case respUserDetails of
        VI.HVAadhaarFlow hvRespDetails -> do
          when (aadhaarReq.maskedAadhaarNumber /= hvRespDetails.idNumber) $ void $ Image.throwValidationError aadhaarReq.aadhaarBackImageId aadhaarReq.aadhaarFrontImageId Nothing
          when (aadhaarReq.nameOnCard /= hvRespDetails.fullName) $ void $ Image.throwValidationError aadhaarReq.aadhaarBackImageId aadhaarReq.aadhaarFrontImageId Nothing
          when (aadhaarReq.dateOfBirth /= hvRespDetails.dob) $ void $ Image.throwValidationError aadhaarReq.aadhaarBackImageId aadhaarReq.aadhaarFrontImageId Nothing
          when (aadhaarReq.address /= hvRespDetails.address) $ void $ Image.throwValidationError aadhaarReq.aadhaarBackImageId aadhaarReq.aadhaarFrontImageId Nothing
        _ -> void $ Image.throwValidationError aadhaarReq.aadhaarBackImageId aadhaarReq.aadhaarFrontImageId Nothing

getDriverRegisterBankAccountLink ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Environment.Flow API.Types.UI.DriverOnboardingV2.BankAccountLinkResp
  )
getDriverRegisterBankAccountLink (mbPersonId, _, _) = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  person <- runInReplica $ PersonQuery.findById personId >>= fromMaybeM (PersonNotFound personId.getId)

  mDriverBankAccount <- runInReplica $ QDBA.findByPrimaryKey personId
  now <- getCurrentTime
  case mDriverBankAccount of
    Just bankAccount -> do
      when bankAccount.chargesEnabled $ throwError $ InvalidRequest "Bank account already enabled"
      case (bankAccount.currentAccountLink, bankAccount.currentAccountLinkExpiry) of
        (Just link, Just expiry) -> do
          if expiry > now
            then
              return $
                API.Types.UI.DriverOnboardingV2.BankAccountLinkResp
                  { chargesEnabled = bankAccount.chargesEnabled,
                    accountLink = link,
                    accountUrlExpiry = expiry,
                    detailsSubmitted = bankAccount.detailsSubmitted
                  }
            else refreshLink person bankAccount
        _ -> refreshLink person bankAccount
    _ -> createAccount person now
  where
    refreshLink :: Domain.Types.Person.Person -> DDBA.DriverBankAccount -> Environment.Flow API.Types.UI.DriverOnboardingV2.BankAccountLinkResp
    refreshLink person bankAccount = do
      resp <- TPayment.retryAccountLink person.merchantId person.merchantOperatingCityId bankAccount.accountId
      accountUrl <- Kernel.Prelude.parseBaseUrl resp.accountUrl
      QDBA.updateAccountLink (Just accountUrl) (Just resp.accountUrlExpiry) person.id
      return $
        API.Types.UI.DriverOnboardingV2.BankAccountLinkResp
          { chargesEnabled = bankAccount.chargesEnabled,
            accountLink = accountUrl,
            accountUrlExpiry = resp.accountUrlExpiry,
            detailsSubmitted = bankAccount.detailsSubmitted
          }

    createAccount :: Domain.Types.Person.Person -> UTCTime -> Environment.Flow API.Types.UI.DriverOnboardingV2.BankAccountLinkResp
    createAccount person now = do
      merchantOpCity <- CQMOC.findById person.merchantOperatingCityId >>= fromMaybeM (MerchantOperatingCityNotFound person.merchantOperatingCityId.getId)
      when (merchantOpCity.country /= Context.USA) $ throwError $ InvalidRequest "Bank account creation is only supported for USA"

      mbMobileNumber <- mapM decrypt person.mobileNumber
      mobileNumber <- mbMobileNumber & fromMaybeM (InvalidRequest "Mobile number is required for opening a bank account")

      driverLicense <- runInReplica $ QDL.findByDriverId person.id >>= fromMaybeM (DriverDLNotFound person.id.getId)
      driverDob <- driverLicense.driverDob & fromMaybeM (InvalidRequest "Driver DOB is required for opening a bank account")
      -- idNumber <- decrypt driverLicense.licenseNumber

      driverSSN <- runInReplica $ QDriverSSN.findByDriverId person.id >>= fromMaybeM (DriverSSNNotFound person.id.getId)
      ssnNumber <- decrypt driverSSN.ssn
      let ssnLast4 = T.takeEnd 4 ssnNumber

      let createAccountReq =
            Payment.IndividualConnectAccountReq
              { country = merchantOpCity.country,
                email = person.email,
                dateOfBirth = DT.utctDay driverDob,
                firstName = person.firstName,
                lastName = person.lastName,
                address = Nothing, -- will add later
                ssnLast4 = Just ssnLast4,
                idNumber = Nothing,
                mobileNumber
              }
      resp <- TPayment.createIndividualConnectAccount person.merchantId person.merchantOperatingCityId createAccountReq
      accountUrl <- Kernel.Prelude.parseBaseUrl resp.accountUrl
      let driverBankAccount =
            DDBA.DriverBankAccount
              { accountId = resp.accountId,
                chargesEnabled = resp.chargesEnabled,
                currentAccountLink = Just accountUrl,
                currentAccountLinkExpiry = Just resp.accountUrlExpiry,
                detailsSubmitted = resp.detailsSubmitted,
                driverId = person.id,
                merchantId = Just person.merchantId,
                merchantOperatingCityId = Just person.merchantOperatingCityId,
                createdAt = now,
                updatedAt = now
              }
      QDBA.create driverBankAccount
      return $
        API.Types.UI.DriverOnboardingV2.BankAccountLinkResp
          { chargesEnabled = resp.chargesEnabled,
            accountLink = accountUrl,
            accountUrlExpiry = resp.accountUrlExpiry,
            detailsSubmitted = resp.detailsSubmitted
          }

getDriverRegisterBankAccountStatus ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Environment.Flow API.Types.UI.DriverOnboardingV2.BankAccountResp
  )
getDriverRegisterBankAccountStatus (mbPersonId, _, _) = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  person <- runInReplica $ PersonQuery.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  driverBankAccount <- runInReplica $ QDBA.findByPrimaryKey personId >>= fromMaybeM (DriverBankAccountNotFound personId.getId)
  if driverBankAccount.chargesEnabled
    then
      return $
        API.Types.UI.DriverOnboardingV2.BankAccountResp
          { chargesEnabled = driverBankAccount.chargesEnabled,
            detailsSubmitted = driverBankAccount.detailsSubmitted
          }
    else do
      resp <- TPayment.getAccount person.merchantId person.merchantOperatingCityId driverBankAccount.accountId
      QDBA.updateAccountStatus resp.chargesEnabled resp.detailsSubmitted personId
      return $
        API.Types.UI.DriverOnboardingV2.BankAccountResp
          { chargesEnabled = resp.chargesEnabled,
            detailsSubmitted = resp.detailsSubmitted
          }

postDriverRegisterLogHvSdkCall ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    API.Types.UI.DriverOnboardingV2.HVSdkCallLogReq ->
    Environment.Flow APISuccess
  )
postDriverRegisterLogHvSdkCall (mbDriverId, merchantId, merchantOperatingCityId) APITypes.HVSdkCallLogReq {..} = do
  driverId <- mbDriverId & fromMaybeM (PersonNotFound "No person found")
  HVSdkLogsQuery.create =<< makeHyperVergeSdkLogs driverId
  return Success
  where
    makeHyperVergeSdkLogs driverId = do
      now <- getCurrentTime
      return $
        DomainHVSdkLogs.HyperVergeSdkLogs
          { createdAt = now,
            updatedAt = now,
            ..
          }
