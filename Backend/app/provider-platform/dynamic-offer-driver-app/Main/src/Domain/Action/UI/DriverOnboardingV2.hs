module Domain.Action.UI.DriverOnboardingV2 where

import qualified API.Types.UI.DriverOnboardingV2
import qualified API.Types.UI.DriverOnboardingV2 as APITypes
import qualified AWS.S3 as S3
import qualified Control.Monad.Extra as CME
import Crypto.Random (getRandomBytes)
import qualified Data.List as DL
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (defaultTimeLocale, formatTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import qualified Domain.Action.UI.DriverOnboarding.Image as Image
import qualified Domain.Action.UI.DriverOnboarding.PullDocument as PullDocument
import qualified Domain.Action.UI.DriverOnboarding.VehicleRegistrationCertificate as VRC
import qualified Domain.Types.AadhaarCard
import Domain.Types.BackgroundVerification
import Domain.Types.Common
import qualified Domain.Types.CommonDriverOnboardingDocuments
import qualified Domain.Types.DigilockerVerification as DDV
import qualified Domain.Types.DocStatus as DocStatus
import qualified Domain.Types.DocumentVerificationConfig
import qualified Domain.Types.DocumentVerificationConfig as DTO
import qualified Domain.Types.DocumentVerificationConfig as Domain
import qualified Domain.Types.DriverGstin as DGST
import qualified Domain.Types.DriverPanCard as DPC
import Domain.Types.DriverSSN
import Domain.Types.FarePolicy
import qualified Domain.Types.HyperVergeSdkLogs as DomainHVSdkLogs
import qualified Domain.Types.Image as Image
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.MerchantPaymentMethod as DMPM
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
import Kernel.External.Maps (LatLong (..))
import Kernel.External.Types (Language (..), ServiceFlow)
import qualified Kernel.External.Verification.Interface as VI
import qualified Kernel.External.Verification.Interface.Types as Verification
import qualified Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.APISuccess
import Kernel.Types.Beckn.DecimalValue as DecimalValue
import qualified Kernel.Types.Documents as Documents
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.DriverOnboarding
import qualified SharedLogic.DriverOnboarding as SDO
import SharedLogic.DriverOnboarding.Digilocker
  ( base64UrlEncodeNoPadding,
    constructDigiLockerAuthUrl,
    generateCodeChallenge,
    getDigiLockerConfig,
    verifyDigiLockerEnabled,
  )
import qualified SharedLogic.DriverOnboarding.Digilocker as DigilockerLockerShared
import SharedLogic.FareCalculator
import SharedLogic.FarePolicy
import qualified SharedLogic.Merchant as SMerchant
import qualified SharedLogic.PersonBankAccount as SPBA
import SharedLogic.VehicleServiceTier
import qualified Storage.Cac.MerchantServiceUsageConfig as CQMSUC
import qualified Storage.Cac.TransporterConfig as CQTC
import qualified Storage.CachedQueries.DocumentVerificationConfig as CQDVC
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.VehicleServiceTier as CQVST
import qualified Storage.Queries.AadhaarCard as QAadhaarCard
import qualified Storage.Queries.BackgroundVerification as QBV
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.CommonDriverOnboardingDocuments as QCommonDriverOnboardingDocuments
import qualified Storage.Queries.DigilockerVerification as QDV
import qualified Storage.Queries.DriverGstin as QDGTIN
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.DriverLicense as QDL
import qualified Storage.Queries.DriverPanCard as QDPC
import qualified Storage.Queries.DriverRCAssociation as DAQuery
import qualified Storage.Queries.DriverSSN as QDriverSSN
import qualified Storage.Queries.FleetDriverAssociationExtra as FDA
import qualified Storage.Queries.HyperVergeSdkLogs as HVSdkLogsQuery
import qualified Storage.Queries.Image as ImageQuery
import qualified Storage.Queries.Person as PersonQuery
import qualified Storage.Queries.QueriesExtra.RideLite as QRideLite
import qualified Storage.Queries.Translations as MTQuery
import qualified Storage.Queries.Vehicle as QVehicle
import qualified Storage.Queries.VehicleRegistrationCertificate as RCQuery
import qualified Storage.Queries.VehicleRegistrationCertificateExtra as VRCE
import qualified Tools.BackgroundVerification as BackgroundVerificationT
import Tools.Error
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
        isMandatoryForEnabling = fromMaybe isMandatory isMandatoryForEnabling,
        applicableTo = applicableTo,
        documentFields = documentFields,
        documentFlowGrouping = fromMaybe Domain.STANDARD documentFlowGrouping,
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
  let personLanguage = fromMaybe ENGLISH person.language
  getOnboardingConfigs' personLanguage merchantOpCityId makeSelfieAadhaarPanMandatory mbOnlyVehicle

getOnboardingConfigs' ::
  Language ->
  Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity ->
  Kernel.Prelude.Maybe Kernel.Prelude.Bool ->
  Kernel.Prelude.Maybe Kernel.Prelude.Bool ->
  Environment.Flow API.Types.UI.DriverOnboardingV2.DocumentVerificationConfigList
getOnboardingConfigs' personLanguage merchantOpCityId makeSelfieAadhaarPanMandatory mbOnlyVehicle = do
  cabConfigsRaw <- CQDVC.findByMerchantOpCityIdAndCategory merchantOpCityId DVC.CAR Nothing
  autoConfigsRaw <- CQDVC.findByMerchantOpCityIdAndCategory merchantOpCityId DVC.AUTO_CATEGORY Nothing
  bikeConfigsRaw <- CQDVC.findByMerchantOpCityIdAndCategory merchantOpCityId DVC.MOTORCYCLE Nothing
  ambulanceConfigsRaw <- CQDVC.findByMerchantOpCityIdAndCategory merchantOpCityId DVC.AMBULANCE Nothing
  truckConfigsRaw <- CQDVC.findByMerchantOpCityIdAndCategory merchantOpCityId DVC.TRUCK Nothing
  boatConfigsRaw <- CQDVC.findByMerchantOpCityIdAndCategory merchantOpCityId DVC.BOAT Nothing
  busConfigsRaw <- CQDVC.findByMerchantOpCityIdAndCategory merchantOpCityId DVC.BUS Nothing
  totoConfigsRaw <- CQDVC.findByMerchantOpCityIdAndCategory merchantOpCityId DVC.TOTO Nothing
  cabConfigs <- SDO.filterInCompatibleFlows makeSelfieAadhaarPanMandatory <$> mapM (mkDocumentVerificationConfigAPIEntity personLanguage) (SDO.filterVehicleDocuments cabConfigsRaw mbOnlyVehicle)
  autoConfigs <- SDO.filterInCompatibleFlows makeSelfieAadhaarPanMandatory <$> mapM (mkDocumentVerificationConfigAPIEntity personLanguage) (SDO.filterVehicleDocuments autoConfigsRaw mbOnlyVehicle)
  bikeConfigs <- SDO.filterInCompatibleFlows makeSelfieAadhaarPanMandatory <$> mapM (mkDocumentVerificationConfigAPIEntity personLanguage) (SDO.filterVehicleDocuments bikeConfigsRaw mbOnlyVehicle)
  ambulanceConfigs <- SDO.filterInCompatibleFlows makeSelfieAadhaarPanMandatory <$> mapM (mkDocumentVerificationConfigAPIEntity personLanguage) (SDO.filterVehicleDocuments ambulanceConfigsRaw mbOnlyVehicle)
  truckConfigs <- SDO.filterInCompatibleFlows makeSelfieAadhaarPanMandatory <$> mapM (mkDocumentVerificationConfigAPIEntity personLanguage) (SDO.filterVehicleDocuments truckConfigsRaw mbOnlyVehicle)
  busConfigs <- SDO.filterInCompatibleFlows makeSelfieAadhaarPanMandatory <$> mapM (mkDocumentVerificationConfigAPIEntity personLanguage) (SDO.filterVehicleDocuments busConfigsRaw mbOnlyVehicle)
  boatConfigs <- SDO.filterInCompatibleFlows makeSelfieAadhaarPanMandatory <$> mapM (mkDocumentVerificationConfigAPIEntity personLanguage) (SDO.filterVehicleDocuments boatConfigsRaw mbOnlyVehicle)
  totoConfigs <- SDO.filterInCompatibleFlows makeSelfieAadhaarPanMandatory <$> mapM (mkDocumentVerificationConfigAPIEntity personLanguage) (SDO.filterVehicleDocuments totoConfigsRaw mbOnlyVehicle)
  return $
    API.Types.UI.DriverOnboardingV2.DocumentVerificationConfigList
      { cabs = SDO.toMaybe cabConfigs,
        autos = SDO.toMaybe autoConfigs,
        bikes = SDO.toMaybe bikeConfigs,
        ambulances = SDO.toMaybe ambulanceConfigs,
        trucks = SDO.toMaybe truckConfigs,
        bus = SDO.toMaybe busConfigs,
        boat = SDO.toMaybe boatConfigs,
        toto = SDO.toMaybe totoConfigs
      }

getDriverVehiclePhotos ::
  ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
    Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
  ) ->
  Text ->
  Environment.Flow API.Types.UI.DriverOnboardingV2.VehiclePhotosResp
getDriverVehiclePhotos (_, merchantId, _) rcNo = do
  encryptedRC <- encrypt rcNo
  rc <- VRCE.findByRC encryptedRC >>= fromMaybeM (RCNotFound rcNo)
  odometer <- getVehicleImages rc Domain.Odometer
  front <- getVehicleImages rc Domain.VehicleFront
  back <- getVehicleImages rc Domain.VehicleBack
  right <- getVehicleImages rc Domain.VehicleRight
  left <- getVehicleImages rc Domain.VehicleLeft
  frontInterior <- getVehicleImages rc Domain.VehicleFrontInterior
  backInterior <- getVehicleImages rc Domain.VehicleBackInterior
  return API.Types.UI.DriverOnboardingV2.VehiclePhotosResp {..}
  where
    getVehicleImages rc imageType = map (.s3Path) <$> runInReplica (ImageQuery.findImagesByRCAndType merchantId (Just rc.id.getId) imageType Nothing)

getDriverVehiclePhotosB64 ::
  ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
    Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
  ) ->
  Kernel.Prelude.Maybe Kernel.Prelude.Bool ->
  Kernel.Prelude.Maybe Kernel.Prelude.Bool ->
  Kernel.Prelude.Maybe Kernel.Prelude.Bool ->
  Kernel.Prelude.Maybe Kernel.Prelude.Bool ->
  Kernel.Prelude.Maybe Kernel.Prelude.Bool ->
  Kernel.Prelude.Maybe Kernel.Prelude.Bool ->
  Kernel.Prelude.Maybe Kernel.Prelude.Bool ->
  Kernel.Prelude.Maybe Kernel.Prelude.Bool ->
  Kernel.Prelude.Text ->
  Environment.Flow API.Types.UI.DriverOnboardingV2.VehiclePhotosResp
getDriverVehiclePhotosB64 (_, merchantId, _) back_ backInterior_ front_ frontInterior_ left_ odometer_ onlyLatest right_ rcNo = do
  encryptedRC <- encrypt rcNo
  rc <- VRCE.findByRC encryptedRC >>= fromMaybeM (RCNotFound rcNo)
  odometer <- getVehicleImagesB64 rc Domain.Odometer odometer_
  front <- getVehicleImagesB64 rc Domain.VehicleFront front_
  back <- getVehicleImagesB64 rc Domain.VehicleBack back_
  right <- getVehicleImagesB64 rc Domain.VehicleRight right_
  left <- getVehicleImagesB64 rc Domain.VehicleLeft left_
  frontInterior <- getVehicleImagesB64 rc Domain.VehicleFrontInterior frontInterior_
  backInterior <- getVehicleImagesB64 rc Domain.VehicleBackInterior backInterior_
  return API.Types.UI.DriverOnboardingV2.VehiclePhotosResp {..}
  where
    getVehicleImagesB64 rc imageType shouldFetch = do
      if fromMaybe False shouldFetch
        then do
          images <- runInReplica $ ImageQuery.findImagesByRCAndType merchantId (Just rc.id.getId) imageType $ if fromMaybe True onlyLatest then Just 1 else Nothing
          mapM (\img -> S3.get $ T.unpack img.s3Path) images
        else pure []

getDriverRateCard ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Prelude.Maybe Meters ->
    Kernel.Prelude.Maybe Minutes ->
    Kernel.Prelude.Maybe Domain.Types.Common.TripCategory ->
    Kernel.Prelude.Maybe Domain.Types.Common.ServiceTierType ->
    Environment.Flow [API.Types.UI.DriverOnboardingV2.RateCardResp]
  )
getDriverRateCard (mbPersonId, _, merchantOperatingCityId) reqDistance reqDuration mbTripCategoryQuery mbServiceTierType = do
  (currency, distanceUnit) <- SMerchant.getCurrencyAndDistanceUnitByMerchantOpCity merchantOperatingCityId
  let mbDistance = reqDistance
      mbDuration = reqDuration
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  transporterConfig <- CQTC.findByMerchantOpCityId merchantOperatingCityId (Just (DriverId (cast personId)))
  driverInfo <- runInReplica $ QDI.findById personId >>= fromMaybeM DriverInfoNotFound
  vehicle <- runInReplica $ QVehicle.findById personId >>= fromMaybeM (VehicleNotFound personId.getId)
  -- driverStats <- runInReplica $ QDriverStats.findById personId >>= fromMaybeM DriverInfoNotFound
  cityVehicleServiceTiers <- CQVST.findAllByMerchantOpCityId merchantOperatingCityId Nothing
  (mbTripCategory, mbPickup, mbVehicleServiceType) <-
    if driverInfo.onRide
      then do
        ride <- QRideLite.findInProgressByDriverId personId >>= fromMaybeM (RideNotFound $ "Driver On Ride but ride not found for driverid:" <> personId.getId)
        booking <- QBooking.findById ride.bookingId >>= fromMaybeM (BookingNotFound $ "Booking not found for ride booking id:" <> ride.bookingId.getId)
        pure (ride.tripCategory, Just $ LatLong booking.fromLocation.lat booking.fromLocation.lon, Just booking.vehicleServiceTier)
      else pure (Nothing, Nothing, Nothing)
  let driverVehicleServiceTierTypes = (\(vehicleServiceTier, _) -> vehicleServiceTier.serviceTierType) <$> selectVehicleTierForDriverWithUsageRestriction False driverInfo vehicle cityVehicleServiceTiers
  let mbServiceTierType' = mbServiceTierType <|> mbVehicleServiceType
  let tripCategory = fromMaybe (OneWay OneWayOnDemandDynamicOffer) (mbTripCategoryQuery <|> mbTripCategory)
  case mbServiceTierType' of
    Just serviceTierType -> do
      when (serviceTierType `notElem` driverVehicleServiceTierTypes) $ throwError $ InvalidRequest ("Service tier " <> show serviceTierType <> " not available for driver")
      mbRateCard <- getRateCardForServiceTier mbDistance mbDuration mbPickup transporterConfig tripCategory distanceUnit currency serviceTierType
      return $ maybeToList mbRateCard
    Nothing -> do
      rateCards <- mapM (getRateCardForServiceTier mbDistance mbDuration mbPickup transporterConfig tripCategory distanceUnit currency) driverVehicleServiceTierTypes
      return $ catMaybes rateCards
  where
    mkBreakupItem :: Currency -> Text -> Text -> Maybe API.Types.UI.DriverOnboardingV2.RateCardItem
    mkBreakupItem currency title valueInText = do
      priceObject <- stringToPrice currency valueInText
      return $
        API.Types.UI.DriverOnboardingV2.RateCardItem
          { title,
            price = priceObject.amountInt,
            priceWithCurrency = mkPriceAPIEntity priceObject
          }

    getRateCardForServiceTier :: Maybe Meters -> Maybe Minutes -> Maybe LatLong -> Maybe TransporterConfig -> TripCategory -> DistanceUnit -> Currency -> Domain.Types.Common.ServiceTierType -> Environment.Flow (Maybe API.Types.UI.DriverOnboardingV2.RateCardResp)
    getRateCardForServiceTier mbDistance mbDuration mbPickupLatLon transporterConfig tripCategory distanceUnit currency serviceTierType = do
      now <- getCurrentTime
      eitherFullFarePolicy <-
        withTryCatch "getFarePolicyWithServiceTierType" (getFarePolicy mbPickupLatLon Nothing Nothing Nothing Nothing Nothing merchantOperatingCityId False tripCategory serviceTierType Nothing Nothing Nothing Nothing [])
          >>= \case
            Left _ -> withTryCatch "getOneWayOnDemandFarePolicy" $ getFarePolicy Nothing Nothing Nothing Nothing Nothing Nothing merchantOperatingCityId False (Delivery OneWayOnDemandDynamicOffer) serviceTierType Nothing Nothing Nothing Nothing []
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
                  driverSelectedFare = Nothing,
                  petCharges = Nothing,
                  customerExtraFee = Nothing,
                  nightShiftCharge = Nothing,
                  customerCancellationDues = Nothing,
                  nightShiftOverlapChecking = isFixedNightCharge tripCategory,
                  estimatedDistance = mbDistance,
                  estimatedRideDuration = minutesToSeconds <$> mbDuration,
                  timeDiffFromUtc = transporterConfig <&> (.timeDiffFromUtc),
                  currency,
                  distanceUnit,
                  shouldApplyBusinessDiscount = False,
                  shouldApplyPersonalDiscount = True,
                  tollCharges = Nothing,
                  merchantOperatingCityId = Just merchantOperatingCityId,
                  mbAdditonalChargeCategories = Nothing,
                  numberOfLuggages = Nothing
                }
          let totalFareAmount = perRideKmFareParamsSum fareParams
              perKmAmount :: Rational = totalFareAmount.getHighPrecMoney / fromIntegral (maybe 1 (getKilometers . metersToKilometers) mbDistance)
              perKmRate =
                PriceAPIEntity
                  { amount = HighPrecMoney perKmAmount,
                    currency
                  }
              totalFare =
                PriceAPIEntity
                  { amount = totalFareAmount,
                    currency
                  }
              perMinuteRate = getPerMinuteRate fareParams
          let rateCardItems = catMaybes $ mkFarePolicyBreakups EulerHS.Prelude.id (mkBreakupItem currency) Nothing Nothing Nothing totalFare.amount Nothing (fullFarePolicyToFarePolicy fullFarePolicy)
          return $
            Just $
              API.Types.UI.DriverOnboardingV2.RateCardResp
                { serviceTierType,
                  perKmRate,
                  totalFare,
                  perMinuteRate,
                  tripCategory,
                  farePolicyHour = if isPeak then APITypes.Peak else if isNight then APITypes.Night else APITypes.NonPeak,
                  rateCardItems,
                  farePolicyId = fullFarePolicy.id,
                  fareParams = fareParams
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
  cityVehicleServiceTiers <- CQVST.findAllByMerchantOpCityId merchantOperatingCityId (Just [])
  SDO.checkAndUpdateAirConditioned False isAirConditioned personId cityVehicleServiceTiers Nothing
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
  cityVehicleServiceTiers <- CQVST.findAllByMerchantOpCityId merchantOpCityId (Just [])
  let personLanguage = fromMaybe ENGLISH person.language

  let driverVehicleServiceTierTypes = selectVehicleTierForDriverWithUsageRestriction False driverInfo vehicle cityVehicleServiceTiers
  let serviceTierACThresholds = map (\(VehicleServiceTier {..}, _) -> airConditionedThreshold) driverVehicleServiceTierTypes
  let isACCheckEnabledForCity = any isJust serviceTierACThresholds
  let isACAllowedForDriver = SDO.checkIfACAllowedForDriver driverInfo (catMaybes serviceTierACThresholds)
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
            then MTQuery.findByErrorAndLanguage "AC_RESTRICTION_MESSAGE" personLanguage
            else
              if isACWorkingForVehicle
                then MTQuery.findByErrorAndLanguage "AC_WORKING_MESSAGE" personLanguage
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
      canSwitchToIntraCity' = any (\(st, _) -> st.vehicleCategory == Just DVC.CAR) driverVehicleServiceTierTypes && (canSwitchToInterCity' || canSwitchToRental')

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
  cityVehicleServiceTiers <- CQVST.findAllByMerchantOpCityId merchantOperatingCityId (Just [])

  whenJust airConditioned $ \ac -> SDO.checkAndUpdateAirConditioned False ac.isWorking personId cityVehicleServiceTiers Nothing
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
        if any (\(st, _) -> st.vehicleCategory == Just DVC.CAR) driverVehicleServiceTierTypes && (canSwitchToInterCity' || canSwitchToRental')
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
postDriverRegisterPancard (mbPersonId, merchantId, merchantOpCityId) req = postDriverRegisterPancardHelper (mbPersonId, merchantId, merchantOpCityId) False False req

postDriverRegisterPancardHelper ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Bool -> -- isDashboard
    Bool -> -- isDigiLockerFlow (server-controlled flag to skip verification for DigiLocker-verified documents)
    API.Types.UI.DriverOnboardingV2.DriverPanReq ->
    Flow APISuccess
  )
postDriverRegisterPancardHelper (mbPersonId, merchantId, merchantOpCityId) isDashboard isDigiLockerFlow req = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  person <- runInReplica $ PersonQuery.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  merchantServiceUsageConfig <-
    CQMSUC.findByMerchantOpCityId merchantOpCityId Nothing
      >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantOpCityId.getId)
  let mbPanVerificationService =
        (if isDashboard then merchantServiceUsageConfig.dashboardPanVerificationService else merchantServiceUsageConfig.panVerificationService)

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
  verificationStatus <-
    if isDigiLockerFlow
      then pure Documents.VALID
      else case mbPanVerificationService of
        Just VI.HyperVerge -> do
          callHyperVerge
        Just VI.Idfy -> do
          callIdfy person.id.getId
        _ -> pure Documents.VALID

  QDPC.upsertPanRecord =<< buildPanCard merchantId person req verificationStatus (Just merchantOpCityId)
  return Success
  where
    getImage :: Id Image.Image -> Flow Text
    getImage imageId = do
      imageMetadata <- ImageQuery.findById imageId >>= fromMaybeM (ImageNotFound imageId.getId)
      unless (imageMetadata.verificationStatus == Just Documents.VALID) $ throwError (ImageNotValid imageId.getId)
      unless (imageMetadata.imageType == DTO.PanCard) $
        throwError (ImageInvalidType (show DTO.PanCard) "")
      Redis.withLockRedisAndReturnValue (VRC.imageS3Lock (imageMetadata.s3Path)) 5 $
        S3.get $ T.unpack imageMetadata.s3Path
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
    callHyperVerge :: Flow Documents.VerificationStatus
    callHyperVerge = do
      void $ checkIfGenuineReq req
      pure Documents.VALID
    callIdfy :: Text -> Flow Documents.VerificationStatus
    callIdfy personId = do
      image1 <- getImage req.imageId1
      resp <-
        Verification.extractPanImage merchantId merchantOpCityId $
          Verification.ExtractImageReq
            { image1 = image1,
              image2 = Nothing,
              driverId = personId
            }
      logDebug $ show resp
      case resp.extractedPan of
        Just extractedPan -> do
          let extractedPanNo = removeSpaceAndDash <$> extractedPan.id_number
          unless (extractedPanNo == Just req.panNumber) $
            throwError $ InvalidRequest "Invalid Image, PAN number not matching."
          pure Documents.VALID
        Nothing -> throwError $ InvalidRequest "Invalid PAN image"

buildPanCard ::
  Kernel.Types.Id.Id Domain.Types.Merchant.Merchant ->
  Domain.Types.Person.Person ->
  API.Types.UI.DriverOnboardingV2.DriverPanReq ->
  Documents.VerificationStatus ->
  Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity) ->
  Environment.Flow DPC.DriverPanCard
buildPanCard merchantId person API.Types.UI.DriverOnboardingV2.DriverPanReq {..} verificationStatus merchantOperatingCityId = do
  now <- getCurrentTime
  id <- generateGUID
  encryptedPan <- encrypt panNumber
  return
    DPC.DriverPanCard
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

postDriverRegisterGstin ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    API.Types.UI.DriverOnboardingV2.DriverGstinReq ->
    Environment.Flow Kernel.Types.APISuccess.APISuccess
  )
postDriverRegisterGstin (mbPersonId, merchantId, merchantOpCityId) req = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  person <- runInReplica $ PersonQuery.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  merchantServiceUsageConfig <-
    CQMSUC.findByMerchantOpCityId merchantOpCityId Nothing
      >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantOpCityId.getId)
  let mbGstVerificationService = merchantServiceUsageConfig.gstVerificationService

  mbGstInfo <- QDGTIN.findUnInvalidByGstNumber req.gstNumber
  whenJust mbGstInfo $ \gstInfo -> do
    when (gstInfo.driverId /= personId) $ do
      ImageQuery.deleteById req.imageId1
      throwError $ DocumentAlreadyLinkedToAnotherDriver "GSTIN"
    when (gstInfo.verificationStatus == Documents.MANUAL_VERIFICATION_REQUIRED) $ do
      ImageQuery.deleteById req.imageId1
      throwError $ DocumentUnderManualReview "GSTIN"
    when (gstInfo.verificationStatus == Documents.VALID) $ do
      ImageQuery.deleteById req.imageId1
      throwError $ DocumentAlreadyValidated "GSTIN"
  verificationStatus <- case mbGstVerificationService of
    Just VI.Idfy -> do
      callIdfy person.id.getId
    _ -> pure Documents.VALID

  QDGTIN.upsertGstinRecord =<< buildGstCard merchantId person req verificationStatus (Just merchantOpCityId)
  return Success
  where
    getImage :: Id Image.Image -> Flow Text
    getImage imageId = do
      imageMetadata <- ImageQuery.findById imageId >>= fromMaybeM (ImageNotFound imageId.getId)
      unless (imageMetadata.verificationStatus == Just Documents.VALID) $ throwError (ImageNotValid imageId.getId)
      unless (imageMetadata.imageType == DTO.GSTCertificate) $
        throwError (ImageInvalidType (show DTO.GSTCertificate) "")
      Redis.withLockRedisAndReturnValue (VRC.imageS3Lock (imageMetadata.s3Path)) 5 $
        S3.get $ T.unpack imageMetadata.s3Path
    callIdfy :: Text -> Flow Documents.VerificationStatus
    callIdfy personId = do
      image1 <- getImage req.imageId1
      resp <-
        Verification.extractGSTImage merchantId merchantOpCityId $
          Verification.ExtractImageReq
            { image1 = image1,
              image2 = Nothing,
              driverId = personId
            }
      logDebug $ show resp
      case resp.extractedGST of
        Just extractedGst -> do
          let extractedGstNo = removeSpaceAndDash <$> extractedGst.gstin
          unless (extractedGstNo == Just req.gstNumber) $
            throwError $ InvalidRequest "Inavlid Image, gst number not matching."
          pure Documents.VALID
        Nothing -> throwError $ InvalidRequest "Invalid gst image"

buildGstCard ::
  Kernel.Types.Id.Id Domain.Types.Merchant.Merchant ->
  Domain.Types.Person.Person ->
  API.Types.UI.DriverOnboardingV2.DriverGstinReq ->
  Documents.VerificationStatus ->
  Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity) ->
  Environment.Flow DGST.DriverGstin
buildGstCard merchantId person API.Types.UI.DriverOnboardingV2.DriverGstinReq {..} verificationStatus merchantOperatingCityId = do
  now <- getCurrentTime
  id <- generateGUID
  encryptedGst <- encrypt gstNumber
  return
    DGST.DriverGstin
      { documentImageId1 = imageId1,
        documentImageId2 = imageId2,
        driverId = person.id,
        driverName = Just person.firstName,
        address = Nothing,
        constitutionOfBusiness = Nothing,
        merchantOperatingCityId = merchantOperatingCityId,
        legalName = Nothing,
        tradeName = Nothing,
        typeOfRegistration = Nothing,
        dateOfLiability = Nothing,
        isProvisional = Nothing,
        validFrom = Nothing,
        validUpto = Nothing,
        panNumber = Nothing,
        id = id,
        gstin = encryptedGst,
        merchantId = Just merchantId,
        createdAt = now,
        updatedAt = now,
        verificationStatus = verificationStatus,
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
postDriverRegisterAadhaarCard (mbPersonId, merchantId, merchantOperatingCityId) req = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")

  -- SDK validation (HyperVerge-specific)
  checkIfGenuineReq req

  -- Duplicate/status checks
  validateAadhaarChecks personId

  -- Create and store Aadhaar record
  createAadhaarRecord personId merchantId merchantOperatingCityId req

  return Success
  where
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

-- | Validate Aadhaar checks (duplicate and status checks)
-- Separated from SDK validation for DigiLocker flow
validateAadhaarChecks ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id Domain.Types.Person.Person ->
  m ()
validateAadhaarChecks personId = do
  prevTry <- QAadhaarCard.findByPrimaryKey personId
  whenJust prevTry $ \aadhaarEntity -> do
    when (aadhaarEntity.verificationStatus == Documents.MANUAL_VERIFICATION_REQUIRED) $
      throwError $ DocumentUnderManualReview "Aadhaar"
    when (aadhaarEntity.verificationStatus == Documents.VALID) $
      throwError $ DocumentAlreadyValidated "Aadhaar"

-- | Create and store Aadhaar record
-- Can be called independently from DigiLocker (without SDK validation)
createAadhaarRecord ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id Domain.Types.Person.Person ->
  Id Domain.Types.Merchant.Merchant ->
  Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity ->
  API.Types.UI.DriverOnboardingV2.AadhaarCardReq -> -- Request with all fields
  m ()
createAadhaarRecord personId merchantId merchantOperatingCityId API.Types.UI.DriverOnboardingV2.AadhaarCardReq {..} = do
  currTime <- getCurrentTime
  let verificationStatus = Image.convertValidationStatusToVerificationStatus validationStatus
  let aadhaarCard =
        Domain.Types.AadhaarCard.AadhaarCard
          { driverId = personId,
            merchantId = merchantId,
            merchantOperatingCityId = merchantOperatingCityId,
            verificationStatus = verificationStatus,
            createdAt = currTime,
            updatedAt = currTime,
            aadhaarNumberHash = Nothing,
            driverGender = Nothing,
            driverImage = Nothing,
            driverImagePath = Nothing,
            ..
          }
  -- Uses fields from AadhaarCardReq: aadhaarFrontImageId, aadhaarBackImageId, maskedAadhaarNumber, nameOnCard, dateOfBirth, address, consent, consentTimestamp

  QAadhaarCard.upsertAadhaarRecord aadhaarCard

getDriverRegisterBankAccountLink ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Maybe DMPM.PaymentMode ->
    Environment.Flow API.Types.UI.DriverOnboardingV2.BankAccountLinkResp
  )
getDriverRegisterBankAccountLink (mbPersonId, _, _) paymentMode = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  person <- runInReplica $ PersonQuery.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  let fetchPersonStripeInfo = do
        driverLicense <- runInReplica $ QDL.findByDriverId person.id >>= fromMaybeM (DriverDLNotFound person.id.getId)
        pure
          SPBA.PersonStripeInfo
            { personDob = driverLicense.driverDob,
              address = Nothing, -- will add later
              idNumber = Nothing -- will add later
            }
  let driverRegisterBankAccountLinkHandle = SPBA.PersonRegisterBankAccountLinkHandle {fetchPersonStripeInfo}
  SPBA.getPersonRegisterBankAccountLink driverRegisterBankAccountLinkHandle paymentMode person

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
  SPBA.getPersonRegisterBankAccountStatus person

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

postDriverRegisterCommonDocument ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    API.Types.UI.DriverOnboardingV2.CommonDocumentReq ->
    Environment.Flow APISuccess
  )
postDriverRegisterCommonDocument (mbDriverId, merchantId, merchantOperatingCityId) APITypes.CommonDocumentReq {..} = do
  driverId <- mbDriverId & fromMaybeM (PersonNotFound "No person found")

  -- Validate that imageId exists if provided
  whenJust imageId $ \imgId -> do
    mbImage <- ImageQuery.findById imgId
    whenNothing_ mbImage $ throwError (InvalidRequest "Image not found")

  -- Create the common document entry
  documentEntry <- buildCommonDocument driverId
  logInfo $ "documentEntry: " <> show documentEntry
  QCommonDriverOnboardingDocuments.create documentEntry
  return Success
  where
    buildCommonDocument driverId = do
      id <- generateGUID
      now <- getCurrentTime
      return $
        Domain.Types.CommonDriverOnboardingDocuments.CommonDriverOnboardingDocuments
          { id = id,
            documentImageId = imageId,
            driverId = Just driverId,
            documentType = documentType,
            documentData = documentData,
            rejectReason = Nothing,
            verificationStatus = Documents.MANUAL_VERIFICATION_REQUIRED,
            merchantOperatingCityId = merchantOperatingCityId,
            merchantId = merchantId,
            createdAt = now,
            updatedAt = now
          }

getDriverFleetRcs :: (Maybe (Id Domain.Types.Person.Person), Id Domain.Types.Merchant.Merchant, Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity) -> Maybe Int -> Maybe Int -> Flow APITypes.FleetRCListRes
getDriverFleetRcs (mbDriverId, _, merchantOpCityId) limit offset = do
  driverId <- mbDriverId & fromMaybeM (PersonNotFound "No person found")
  transporterConfig <- CQTC.findByMerchantOpCityId merchantOpCityId (Just (DriverId (cast driverId))) >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  driverLinkedRcs <- DAQuery.findAllLinkedByDriverId driverId
  let driverRcIds = map (.rcId) driverLinkedRcs
  driverRcs <- RCQuery.findAllById driverRcIds
  fleetRcs <-
    if transporterConfig.allowDriverToUseFleetRcs == Just True
      then do
        mbFleetDriverAssociation <- FDA.findByDriverId driverId True
        case mbFleetDriverAssociation of
          Nothing -> pure []
          Just fleetDriverAssociation -> do
            let fleetOwnerId = fleetDriverAssociation.fleetOwnerId
            RCQuery.findAllByFleetOwnerId effectiveLimit offset (Just fleetOwnerId)
      else pure []
  let allRcs = DL.nubBy (\a b -> a.id == b.id) (driverRcs <> fleetRcs)
      activeRcId = fmap (.rcId) . DL.find (.isRcActive) $ driverLinkedRcs
  rcs <- mapM (getCombinedRcData activeRcId) allRcs
  return $ APITypes.FleetRCListRes rcs
  where
    getCombinedRcData activeRcId rc = do
      rcNo <- decrypt rc.certificateNumber
      let isActive = Just rc.id == activeRcId
      return $
        VRC.LinkedRC
          { rcActive = isActive,
            rcDetails = SDO.makeRCAPIEntity rc rcNo,
            isFleetRC = isJust rc.fleetOwnerId
          }
    effectiveLimit = Just $ min 10 (fromMaybe 10 limit)

postDriverLinkToFleet ::
  (Maybe (Id Domain.Types.Person.Person), Id Domain.Types.Merchant.Merchant, Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity) ->
  APITypes.LinkToFleetReq ->
  Flow APISuccess
postDriverLinkToFleet (mbDriverId, _, _) req = do
  driverId <- mbDriverId & fromMaybeM (PersonNotFound "No person found")
  fdaForFleetOwner <- FDA.findByDriverIdAndFleetOwnerIdWithStatus driverId req.fleetOwnerId.getId
  case req.isRevoke of
    Just True -> do
      case fdaForFleetOwner of
        Just fda | not fda.isActive -> FDA.revokeFleetDriverAssociation driverId req.fleetOwnerId
        Just _ -> throwError $ InvalidRequest "Direct revoke is not allowed for active fleet associations"
        Nothing -> throwError $ InvalidRequest "No fleet association found to revoke"
    _ -> do
      case fdaForFleetOwner of
        Just fda | fda.isActive -> throwError $ InvalidRequest "Driver is already linked to this fleet"
        Just _ -> throwError $ InvalidRequest "Driver already has a pending fleet association request with this fleet"
        Nothing -> do
          let requestReason = fromMaybe "Driver requested to join fleet" req.requestReason
          FDA.createFleetDriverAssociationIfNotExists driverId req.fleetOwnerId Nothing (fromMaybe DVC.CAR req.onboardingVehicleCategory) False (Just requestReason)
  return Success

postDriverDigilockerInitiate ::
  ( Maybe (Id Domain.Types.Person.Person),
    Id Domain.Types.Merchant.Merchant,
    Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
  ) ->
  APITypes.DigiLockerInitiateReq ->
  Environment.Flow APITypes.DigiLockerInitiateResp
postDriverDigilockerInitiate (mbDriverId, merchantId, merchantOpCityId) req = do
  driverId <- mbDriverId & fromMaybeM (PersonNotFound "No person found")
  logInfo $ "DigiLocker initiate - Starting authorization flow for DriverId: " <> driverId.getId <> ", VehicleCategory: " <> show req.vehicleCategory

  verifyDigiLockerEnabled merchantOpCityId

  allowedVehicleCategories <- DigilockerLockerShared.getAllowedVehicleCategories merchantOpCityId
  unless (req.vehicleCategory `elem` allowedVehicleCategories) $ do
    let categoriesStr = T.intercalate ", " (map (T.toLower . show) allowedVehicleCategories)
    throwError $ DigiLockerInvalidVehicleCategory $ "Vehicle category must be one of: " <> categoriesStr <> ". Received: " <> show req.vehicleCategory

  latestSession <- QDV.findLatestByDriverId (Just 1) (Just 0) driverId

  case latestSession of
    [] -> do
      logInfo $ "DigiLocker initiate - No existing session found for DriverId: " <> driverId.getId
      createNewDigiLockerSession driverId merchantId merchantOpCityId req.vehicleCategory
    (session : _) -> do
      logInfo $ "DigiLocker initiate - Found existing session for DriverId: " <> driverId.getId <> ", StateId: " <> session.stateId <> ", SessionStatus: " <> show session.sessionStatus
      handleExistingSession driverId merchantId merchantOpCityId session req.vehicleCategory

handleExistingSession ::
  Id Domain.Types.Person.Person ->
  Id Domain.Types.Merchant.Merchant ->
  Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity ->
  DDV.DigilockerVerification ->
  DVC.VehicleCategory ->
  Flow APITypes.DigiLockerInitiateResp
handleExistingSession driverId merchantId merchantOpCityId session vehicleCategory = do
  now <- getCurrentTime

  case session.sessionStatus of
    DDV.PENDING -> handlePendingSession driverId merchantId merchantOpCityId session now
    DDV.SUCCESS -> handleSuccessSession driverId merchantId merchantOpCityId session now vehicleCategory
    DDV.FAILED -> do
      logInfo $ "DigiLocker initiate - Previous session FAILED, creating new session for DriverId: " <> driverId.getId <> ", StateId: " <> session.stateId
      createNewDigiLockerSession driverId merchantId merchantOpCityId vehicleCategory
    DDV.CONSENT_DENIED -> do
      logInfo $ "DigiLocker initiate - Previous session CONSENT_DENIED, creating new session for DriverId: " <> driverId.getId <> ", StateId: " <> session.stateId
      createNewDigiLockerSession driverId merchantId merchantOpCityId vehicleCategory

handlePendingSession ::
  Id Domain.Types.Person.Person ->
  Id Domain.Types.Merchant.Merchant ->
  Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity ->
  DDV.DigilockerVerification ->
  UTCTime ->
  Flow APITypes.DigiLockerInitiateResp
handlePendingSession driverId _merchantId _merchantOpCityId session _now = do
  logInfo $ "DigiLocker initiate - DriverId: " <> driverId.getId <> ", StateId: " <> session.stateId <> ", Session PENDING, returning 409"
  throwError DigiLockerVerificationInProgress

handleSuccessSession ::
  Id Domain.Types.Person.Person ->
  Id Domain.Types.Merchant.Merchant ->
  Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity ->
  DDV.DigilockerVerification ->
  UTCTime ->
  DVC.VehicleCategory ->
  Flow APITypes.DigiLockerInitiateResp
handleSuccessSession driverId merchantId merchantOpCityId session now vehicleCategory = do
  checkDocumentStatuses driverId merchantId merchantOpCityId session now vehicleCategory

checkDocumentStatuses ::
  Id Domain.Types.Person.Person ->
  Id Domain.Types.Merchant.Merchant ->
  Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity ->
  DDV.DigilockerVerification ->
  UTCTime ->
  DVC.VehicleCategory ->
  Flow APITypes.DigiLockerInitiateResp
checkDocumentStatuses driverId merchantId merchantOpCityId session now vehicleCategory = do
  let docStatusMap = session.docStatus

  when (hasDocWithStatus docStatusMap (DocStatus.docStatusToText DocStatus.DOC_PENDING)) $ do
    logInfo $ "DigiLocker initiate - DriverId: " <> driverId.getId <> ", StateId: " <> session.stateId <> ", Found PENDING documents, returning 409"
    throwError DigiLockerDocumentsBeingVerified

  if hasDocWithStatus docStatusMap (DocStatus.docStatusToText DocStatus.DOC_FAILED)
    || hasDocWithStatus docStatusMap (DocStatus.docStatusToText DocStatus.DOC_CONSENT_DENIED)
    then do
      logInfo $ "DigiLocker initiate - DriverId: " <> driverId.getId <> ", StateId: " <> session.stateId <> ", Found FAILED or CONSENT_DENIED documents, creating new session"
      createNewDigiLockerSession driverId merchantId merchantOpCityId vehicleCategory
    else
      if hasDocWithStatus docStatusMap (DocStatus.docStatusToText DocStatus.DOC_PULL_REQUIRED)
        then handlePullRequiredDocs driverId merchantId merchantOpCityId session docStatusMap now vehicleCategory
        else checkActualDocumentTables driverId merchantId merchantOpCityId vehicleCategory

handlePullRequiredDocs ::
  Id Domain.Types.Person.Person ->
  Id Domain.Types.Merchant.Merchant ->
  Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity ->
  DDV.DigilockerVerification ->
  DocStatus.DocStatusMap ->
  UTCTime ->
  DVC.VehicleCategory ->
  Flow APITypes.DigiLockerInitiateResp
handlePullRequiredDocs driverId merchantId merchantOpCityId session _docStatusMap now vehicleCategory = do
  let stateId = session.stateId
  case session.accessTokenExpiresAt of
    Nothing -> do
      logInfo $ "DigiLocker initiate - DriverId: " <> driverId.getId <> ", StateId: " <> stateId <> ", PULL_REQUIRED with no expiry, creating new session"
      createNewDigiLockerSession driverId merchantId merchantOpCityId vehicleCategory
    Just expiresAt ->
      if now < expiresAt
        then do
          logInfo $ "DigiLocker initiate - DriverId: " <> driverId.getId <> ", StateId: " <> stateId <> ", PULL_REQUIRED with valid token, returning 409"
          throwError DigiLockerPullRequired
        else do
          logInfo $ "DigiLocker initiate - DriverId: " <> driverId.getId <> ", StateId: " <> stateId <> ", PULL_REQUIRED with expired token, creating new session"
          createNewDigiLockerSession driverId merchantId merchantOpCityId vehicleCategory

checkActualDocumentTables ::
  Id Domain.Types.Person.Person ->
  Id Domain.Types.Merchant.Merchant ->
  Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity ->
  DVC.VehicleCategory ->
  Flow APITypes.DigiLockerInitiateResp
checkActualDocumentTables driverId merchantId merchantOpCityId vehicleCategory = do
  mbDL <- QDL.findByDriverId driverId
  let dlStatus = mbDL >>= (\dl -> Just dl.verificationStatus)

  mbPan <- QDPC.findByDriverId driverId
  let panStatus = mbPan >>= (\pan -> Just pan.verificationStatus)

  mbAadhaar <- QAadhaarCard.findByPrimaryKey driverId
  let aadhaarStatus = mbAadhaar >>= (\aadhaar -> Just aadhaar.verificationStatus)

  let allStatuses = [dlStatus, panStatus, aadhaarStatus]

  when (any (== Just Documents.PENDING) allStatuses) $ do
    logInfo $ "DigiLocker initiate - DriverId: " <> driverId.getId <> ", Found PENDING documents in tables, returning 409"
    throwError DigiLockerDocumentsBeingVerified

  let allDocumentsValid = all (== Just Documents.VALID) allStatuses
  if allDocumentsValid
    then do
      logInfo $ "DigiLocker initiate - DriverId: " <> driverId.getId <> ", All documents already verified"
      throwError DigiLockerAllDocumentsVerified
    else do
      logInfo $ "DigiLocker initiate - DriverId: " <> driverId.getId <> ", Creating new session for document verification"
      createNewDigiLockerSession driverId merchantId merchantOpCityId vehicleCategory

createNewDigiLockerSession ::
  Id Domain.Types.Person.Person ->
  Id Domain.Types.Merchant.Merchant ->
  Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity ->
  DVC.VehicleCategory ->
  Flow APITypes.DigiLockerInitiateResp
createNewDigiLockerSession driverId merchantId merchantOpCityId vehicleCategory = do
  logInfo $ "DigiLocker initiate - Creating new session for DriverId: " <> driverId.getId <> ", VehicleCategory: " <> show vehicleCategory

  digiLockerConfig <- getDigiLockerConfig merchantOpCityId
  logInfo $ "DigiLocker initiate - Config retrieved for merchantOpCityId: " <> merchantOpCityId.getId

  randomBytes <- liftIO $ getRandomBytes 24
  now <- getCurrentTime
  let timestampMillis = T.pack $ show $ (floor (utcTimeToPOSIXSeconds now * 1000) :: Integer)
  let timestampBytes = TE.encodeUtf8 timestampMillis
  let combinedBytes = randomBytes <> timestampBytes
  let codeVerifier = base64UrlEncodeNoPadding combinedBytes
  let codeChallenge = generateCodeChallenge codeVerifier
  let codeMethod = "S256"

  stateId <- generateGUID

  sessionId <- generateGUID
  let newSession =
        DDV.DigilockerVerification
          { id = sessionId,
            driverId = driverId,
            stateId = stateId,
            codeVerifier = codeVerifier,
            codeChallenge = codeChallenge,
            codeMethod = codeMethod,
            authorizationCode = Nothing,
            accessToken = Nothing,
            accessTokenExpiresAt = Nothing,
            scope = Nothing,
            docStatus = DocStatus.emptyDocStatusMap,
            sessionStatus = DDV.PENDING,
            responseCode = Nothing,
            responseDescription = Nothing,
            vehicleCategory = vehicleCategory,
            merchantId = merchantId,
            merchantOperatingCityId = merchantOpCityId,
            createdAt = now,
            updatedAt = now
          }

  QDV.create newSession

  logInfo $ "DigiLocker initiate - DriverId: " <> driverId.getId <> ", Created session with ID: " <> sessionId.getId <> ", StateId: " <> stateId <> ", VehicleCategory: " <> show vehicleCategory

  let authUrl = constructDigiLockerAuthUrl digiLockerConfig stateId codeChallenge

  return $ APITypes.DigiLockerInitiateResp {authorizationUrl = authUrl}

hasDocWithStatus :: DocStatus.DocStatusMap -> Text -> Bool
hasDocWithStatus docStatusMap targetStatus =
  any (hasStatus targetStatus) (DocStatus.toList docStatusMap)
  where
    hasStatus :: Text -> (Domain.DocumentType, DocStatus.DocumentStatus) -> Bool
    hasStatus target (_, documentStatus) =
      DocStatus.docStatusEnumToText documentStatus.status == target

postDriverDigilockerPullDocuments ::
  ( Maybe (Id Domain.Types.Person.Person),
    Id Domain.Types.Merchant.Merchant,
    Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
  ) ->
  APITypes.PullDocumentReq ->
  Environment.Flow APISuccess
postDriverDigilockerPullDocuments (mbDriverId, merchantId, merchantOpCityId) req = do
  logInfo $ "PullDocuments - Starting pull operation for DocType: " <> show req.docType
  PullDocument.pullDocuments (mbDriverId, merchantId, merchantOpCityId) req
