{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Action.UI.DriverOnboardingV2 where

import qualified API.Types.UI.DriverOnboardingV2
import qualified API.Types.UI.DriverOnboardingV2 as APITypes
import Data.OpenApi (ToSchema)
import Domain.Types.Common
import qualified Domain.Types.DocumentVerificationConfig
import qualified Domain.Types.DocumentVerificationConfig as DTO
import Domain.Types.DriverInformation
import qualified Domain.Types.DriverPanCard as Domain
import Domain.Types.DriverSSN
import Domain.Types.FarePolicy
import qualified Domain.Types.IdfyVerification as DIV
import qualified Domain.Types.Image as Image
import qualified Domain.Types.Merchant
import qualified Domain.Types.Merchant.MerchantOperatingCity
import Domain.Types.Merchant.TransporterConfig
import qualified Domain.Types.Person
import Domain.Types.ServiceTierType
import qualified Domain.Types.Vehicle as DTV
import Domain.Types.VehicleServiceTier
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified EulerHS.Prelude
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.External.Types (Language (..))
import qualified Kernel.Prelude
import Kernel.Types.APISuccess
import Kernel.Types.Beckn.DecimalValue as DecimalValue
import Kernel.Types.Error
import Kernel.Types.Id
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant hiding (throwError)
import SharedLogic.DriverOnboarding
import SharedLogic.FareCalculator
import SharedLogic.FarePolicy
import SharedLogic.VehicleServiceTier
import qualified Storage.Cac.TransporterConfig as CQTC
import qualified Storage.CachedQueries.DocumentVerificationConfig as CQDVC
import qualified Storage.CachedQueries.VehicleServiceTier as CQVST
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.DriverPanCard as QDPC
import qualified Storage.Queries.DriverPanCardExtra as SQDPC
import qualified Storage.Queries.DriverSSN as QDriverSSN
import qualified Storage.Queries.Image as ImageQuery
import qualified Storage.Queries.Person as PersonQuery
import qualified Storage.Queries.Translations as MTQuery
import qualified Storage.Queries.Vehicle as QVehicle
import qualified Storage.Queries.VehicleRegistrationCertificate as QRC
import Tools.Auth
import Tools.Error
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
  mbTitle <- MTQuery.findByErrorAndLanguage ((show documentType) <> "_Title") language
  mbDescription <- MTQuery.findByErrorAndLanguage ((show documentType) <> "_Description") language
  return $
    API.Types.UI.DriverOnboardingV2.DocumentVerificationConfigAPIEntity
      { title = maybe title (.message) mbTitle,
        description = maybe description (Just . (.message)) mbDescription,
        ..
      }

getOnboardingConfigs ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.Merchant.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Prelude.Maybe Kernel.Prelude.Bool ->
    Environment.Flow API.Types.UI.DriverOnboardingV2.DocumentVerificationConfigList
  )
getOnboardingConfigs (mbPersonId, _, merchanOperatingCityId) mbOnlyVehicle = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  person <- runInReplica $ PersonQuery.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  let personLangauge = fromMaybe ENGLISH person.language
  cabConfigsRaw <- CQDVC.findByMerchantOpCityIdAndCategory merchanOperatingCityId DTV.CAR
  autoConfigsRaw <- CQDVC.findByMerchantOpCityIdAndCategory merchanOperatingCityId DTV.AUTO_CATEGORY
  bikeConfigsRaw <- CQDVC.findByMerchantOpCityIdAndCategory merchanOperatingCityId DTV.MOTORCYCLE

  cabConfigs <- mapM (mkDocumentVerificationConfigAPIEntity personLangauge) (filterVehicleDocuments cabConfigsRaw)
  autoConfigs <- mapM (mkDocumentVerificationConfigAPIEntity personLangauge) (filterVehicleDocuments autoConfigsRaw)
  bikeConfigs <- mapM (mkDocumentVerificationConfigAPIEntity personLangauge) (filterVehicleDocuments bikeConfigsRaw)

  return $
    API.Types.UI.DriverOnboardingV2.DocumentVerificationConfigList
      { cabs = toMaybe cabConfigs,
        autos = toMaybe autoConfigs,
        bikes = toMaybe bikeConfigs
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

getDriverRateCard ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.Merchant.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Prelude.Maybe Meters ->
    Kernel.Prelude.Maybe Domain.Types.ServiceTierType.ServiceTierType ->
    Environment.Flow [API.Types.UI.DriverOnboardingV2.RateCardResp]
  )
getDriverRateCard (mbPersonId, _, merchantOperatingCityId) mbDistance mbServiceTierType = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  transporterConfig <- CQTC.findByMerchantOpCityId merchantOperatingCityId (Just (DriverId (cast personId)))
  person <- runInReplica $ PersonQuery.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  driverInfo <- runInReplica $ QDI.findById personId >>= fromMaybeM DriverInfoNotFound
  vehicle <- runInReplica $ QVehicle.findById personId >>= fromMaybeM (VehicleNotFound personId.getId)
  cityVehicleServiceTiers <- CQVST.findAllByMerchantOpCityId merchantOperatingCityId
  let driverVehicleServiceTierTypes = (\(vehicleServiceTier, _) -> vehicleServiceTier.serviceTierType) <$> selectVehicleTierForDriverWithUsageRestriction False person driverInfo vehicle cityVehicleServiceTiers
  case mbServiceTierType of
    Just serviceTierType -> do
      when (serviceTierType `notElem` driverVehicleServiceTierTypes) $ throwError $ InvalidRequest ("Service tier " <> show serviceTierType <> " not available for driver")
      rateCard <- getRateCardForServiceTier transporterConfig (OneWay OneWayOnDemandDynamicOffer) serviceTierType
      return [rateCard]
    Nothing -> do
      rateCard <- mapM (getRateCardForServiceTier transporterConfig (OneWay OneWayOnDemandDynamicOffer)) driverVehicleServiceTierTypes
      return rateCard
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
    getRateCardForServiceTier :: Maybe TransporterConfig -> TripCategory -> Domain.Types.ServiceTierType.ServiceTierType -> Environment.Flow API.Types.UI.DriverOnboardingV2.RateCardResp
    getRateCardForServiceTier transporterConfig tripCategory serviceTierType = do
      now <- getCurrentTime
      fullFarePolicy <- getFarePolicy merchantOperatingCityId (OneWay OneWayOnDemandDynamicOffer) serviceTierType Nothing Nothing
      let rateCardItems = catMaybes $ mkFarePolicyBreakups EulerHS.Prelude.id mkBreakupItem Nothing Nothing (fullFarePolicyToFarePolicy fullFarePolicy)
      let isPeak = maybe False (> 1) fullFarePolicy.congestionChargeMultiplier
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
              actualRideDuration = Nothing,
              avgSpeedOfVehicle = Nothing,
              driverSelectedFare = Nothing,
              customerExtraFee = Nothing,
              nightShiftCharge = Nothing,
              customerCancellationDues = Nothing,
              nightShiftOverlapChecking = isRentalTrip tripCategory,
              estimatedDistance = mbDistance,
              estimatedRideDuration = Nothing,
              timeDiffFromUtc = transporterConfig <&> (.timeDiffFromUtc),
              currency = INR, -- fix it later
              tollCharges = Nothing
            }
      let totalFareAmount = perRideKmFareParamsSum fareParams
      let perKmAmount :: Rational = totalFareAmount.getHighPrecMoney / fromIntegral (maybe 1 (getKilometers . metersToKilometers) mbDistance)
      let perKmRate =
            PriceAPIEntity
              { amount = HighPrecMoney perKmAmount,
                currency = INR
              }
      let totalFare =
            PriceAPIEntity
              { amount = totalFareAmount,
                currency = INR
              }
      return $
        API.Types.UI.DriverOnboardingV2.RateCardResp
          { serviceTierType,
            perKmRate,
            totalFare,
            perMinuteRate = Nothing, -- TODO: Add per minute rate for USA
            tripCategory,
            farePolicyHour = if isPeak then APITypes.Peak else if isNight then APITypes.Night else APITypes.NonPeak,
            rateCardItems
          }

postDriverUpdateAirCondition ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.Merchant.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    API.Types.UI.DriverOnboardingV2.UpdateAirConditionUpdateRequest ->
    Environment.Flow Kernel.Types.APISuccess.APISuccess
  )
postDriverUpdateAirCondition (mbPersonId, _, merchanOperatingCityId) API.Types.UI.DriverOnboardingV2.UpdateAirConditionUpdateRequest {..} = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  cityVehicleServiceTiers <- CQVST.findAllByMerchantOpCityId merchanOperatingCityId
  checkAndUpdateAirConditioned False isAirConditioned personId cityVehicleServiceTiers
  now <- getCurrentTime
  QDI.updateLastACStatusCheckedAt (Just now) personId
  return Success

getDriverVehicleServiceTiers ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.Merchant.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Environment.Flow API.Types.UI.DriverOnboardingV2.DriverVehicleServiceTiers
  )
getDriverVehicleServiceTiers (mbPersonId, _, merchanOperatingCityId) = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  person <- runInReplica $ PersonQuery.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  driverInfo <- runInReplica $ QDI.findById personId >>= fromMaybeM DriverInfoNotFound
  vehicle <- runInReplica $ QVehicle.findById personId >>= fromMaybeM (VehicleNotFound personId.getId)
  cityVehicleServiceTiers <- CQVST.findAllByMerchantOpCityId merchanOperatingCityId
  let personLangauge = fromMaybe ENGLISH person.language

  let driverVehicleServiceTierTypes = selectVehicleTierForDriverWithUsageRestriction False person driverInfo vehicle cityVehicleServiceTiers
  let serviceTierACThresholds = map (\(VehicleServiceTier {..}, _) -> airConditioned) driverVehicleServiceTierTypes
  let isACCheckEnabledForCity = any isJust serviceTierACThresholds
  let isACAllowedForDriver = checkIfACAllowedForDriver driverInfo (catMaybes serviceTierACThresholds)
  let isACWorkingForVehicle = vehicle.airConditioned /= Just False
  let isACWorking = isACAllowedForDriver && isACWorkingForVehicle
  let tierOptions =
        driverVehicleServiceTierTypes <&> \(VehicleServiceTier {..}, usageRestricted) -> do
          let isNonACDefault = isACCheckEnabledForCity && not isACWorking && isNothing airConditioned
          API.Types.UI.DriverOnboardingV2.DriverVehicleServiceTier
            { isSelected = (serviceTierType `elem` vehicle.selectedServiceTiers) || isNonACDefault,
              isDefault = (vehicle.variant `elem` defaultForVehicleVariant) || isNonACDefault,
              isUsageRestricted = Just usageRestricted,
              priority = Just priority,
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

  return $
    API.Types.UI.DriverOnboardingV2.DriverVehicleServiceTiers
      { tiers = tierOptions,
        canSwitchToRental = Just driverInfo.canSwitchToRental,
        canSwitchToInterCity = Just driverInfo.canSwitchToInterCity,
        airConditioned = mbAirConditioned
      }

postDriverUpdateServiceTiers ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.Merchant.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    API.Types.UI.DriverOnboardingV2.DriverVehicleServiceTiers ->
    Environment.Flow APISuccess
  )
postDriverUpdateServiceTiers (mbPersonId, _, merchanOperatingCityId) API.Types.UI.DriverOnboardingV2.DriverVehicleServiceTiers {..} = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  person <- runInReplica $ PersonQuery.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  cityVehicleServiceTiers <- CQVST.findAllByMerchantOpCityId merchanOperatingCityId

  whenJust airConditioned $ \ac -> checkAndUpdateAirConditioned False ac.isWorking personId cityVehicleServiceTiers
  driverInfo <- QDI.findById personId >>= fromMaybeM DriverInfoNotFound
  vehicle <- QVehicle.findById personId >>= fromMaybeM (VehicleNotFound personId.getId)

  let driverVehicleServiceTierTypes = selectVehicleTierForDriverWithUsageRestriction False person driverInfo vehicle cityVehicleServiceTiers
  mbSelectedServiceTiers <-
    driverVehicleServiceTierTypes `forM` \(driverServiceTier, _) -> do
      let isAlreadySelected = driverServiceTier.serviceTierType `elem` vehicle.selectedServiceTiers
          isDefault = vehicle.variant `elem` driverServiceTier.defaultForVehicleVariant
          mbServiceTierDriverRequest = find (\tier -> tier.serviceTierType == driverServiceTier.serviceTierType) tiers
          isSelected = maybe isAlreadySelected (.isSelected) mbServiceTierDriverRequest

      if isSelected || isDefault
        then return $ Just driverServiceTier.serviceTierType
        else return Nothing

  QVehicle.updateSelectedServiceTiers (catMaybes mbSelectedServiceTiers) personId
  when (isJust canSwitchToInterCity || isJust canSwitchToRental) $ do
    let canSwitchToInterCity' = fromMaybe driverInfo.canSwitchToInterCity canSwitchToInterCity
    let canSwitchToRental' = fromMaybe driverInfo.canSwitchToRental canSwitchToRental
    QDI.updateRentalAndInterCitySwitch canSwitchToRental' canSwitchToInterCity' personId

  return Success

postDriverRegisterSsn ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.Merchant.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    API.Types.UI.DriverOnboardingV2.SSNReq ->
    Environment.Flow APISuccess
  )
postDriverRegisterSsn (mbPersonId, _, _) API.Types.UI.DriverOnboardingV2.SSNReq {..} = do
  ssn' <- encrypt ssn
  id' <- generateGUID
  driverId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  QDriverSSN.upsert (buildDriverSSN id' ssn' driverId)
  return Success
  where
    buildDriverSSN id' ssn' driverId' =
      Domain.Types.DriverSSN.DriverSSN
        { id = id',
          driverId = driverId',
          ssn = ssn',
          verificationStatus = DIV.MANUAL_VERIFICATION_REQUIRED,
          rejectReason = Nothing
        }

postDriverRegisterPancard ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.Merchant.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    API.Types.UI.DriverOnboardingV2.DriverPanReq ->
    Environment.Flow Kernel.Types.APISuccess.APISuccess
  )
postDriverRegisterPancard (mbPersonId, merchantId, _) req = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  person <- runInReplica $ PersonQuery.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  mbPanInfo <- SQDPC.findByPanNumber req.panNumber
  getImage req.imageId1 personId ------- Just checking whether the image exists or not
  case mbPanInfo of
    Just pan -> unless (pan.driverId == personId) $ throwImageError req.imageId1 PanAlreadyLinked
    Nothing -> do
      panCard <- buildPanCard merchantId person req
      QDPC.create panCard
  return Success
  where
    getImage :: Kernel.Types.Id.Id Image.Image -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Environment.Flow ()
    getImage imageId personId = do
      imageMetadata <- ImageQuery.findById imageId >>= fromMaybeM (ImageNotFound imageId.getId)
      unless (imageMetadata.isValid) $ throwError (ImageNotValid imageId.getId)
      unless (imageMetadata.personId == personId) $ throwError (ImageNotFound imageId.getId)
      unless (imageMetadata.imageType == DTO.PanCard) $
        throwError (ImageInvalidType (show DTO.PanCard) (show imageMetadata.imageType))

buildPanCard ::
  Kernel.Types.Id.Id Domain.Types.Merchant.Merchant ->
  Domain.Types.Person.Person ->
  API.Types.UI.DriverOnboardingV2.DriverPanReq ->
  Environment.Flow Domain.DriverPanCard
buildPanCard merchantId person API.Types.UI.DriverOnboardingV2.DriverPanReq {..} = do
  now <- getCurrentTime
  id <- generateGUID
  encryptedPan <- encrypt panNumber
  return
    Domain.DriverPanCard
      { consent = consent,
        consentTimestamp = now,
        documentImageId1 = imageId1,
        documentImageId2 = imageId2,
        driverDob = Nothing,
        driverId = person.id,
        driverName = Just person.firstName,
        failedRules = [],
        id = id,
        panCardNumber = encryptedPan,
        verificationStatus = DIV.PENDING,
        merchantId = Just merchantId,
        createdAt = now,
        updatedAt = now
      }
