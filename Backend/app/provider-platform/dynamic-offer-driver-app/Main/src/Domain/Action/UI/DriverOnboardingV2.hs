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
import qualified Domain.Types.CommonDriverOnboardingDocuments
import qualified Domain.Types.DocumentVerificationConfig
import qualified Domain.Types.DocumentVerificationConfig as DTO
import qualified Domain.Types.DocumentVerificationConfig as Domain
import qualified Domain.Types.DriverBankAccount as DDBA
import qualified Domain.Types.DriverGstin as DGST
import qualified Domain.Types.DriverPanCard as DPC
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
import qualified EulerHS.Types as Euler
import Kernel.Beam.Functions
import qualified Kernel.External.BackgroundVerification.Interface as BackgroundVerification
import Kernel.External.Encryption
import Kernel.External.Maps (LatLong (..))
import qualified Kernel.External.Payment.Interface as Payment
import Kernel.External.Types (Language (..), ServiceFlow)
import qualified Kernel.External.Verification.Interface as VI
import qualified Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Beckn.DecimalValue as DecimalValue
import qualified Kernel.Types.Documents as Documents
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Servant
import SharedLogic.DriverOnboarding
import qualified SharedLogic.DriverOnboarding as SDO
import SharedLogic.FareCalculator
import SharedLogic.FarePolicy
import qualified SharedLogic.Merchant as SMerchant
import SharedLogic.VehicleServiceTier
import qualified Storage.Cac.MerchantServiceUsageConfig as CQMSUC
import qualified Storage.Cac.TransporterConfig as CQTC
import qualified Storage.CachedQueries.DocumentVerificationConfig as CQDVC
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.VehicleServiceTier as CQVST
import qualified Storage.Queries.AadhaarCard as QAadhaarCard
import qualified Storage.Queries.BackgroundVerification as QBV
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.CommonDriverOnboardingDocuments as QCommonDriverOnboardingDocuments
import qualified Storage.Queries.DriverBankAccount as QDBA
import qualified Storage.Queries.DriverGstin as QDGTIN
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.DriverLicense as QDL
import qualified Storage.Queries.DriverPanCard as QDPC
import qualified Storage.Queries.DriverRCAssociationExtra as DRAE
import qualified Storage.Queries.DriverSSN as QDriverSSN
import qualified Storage.Queries.HyperVergeSdkLogs as HVSdkLogsQuery
import qualified Storage.Queries.Image as ImageQuery
import qualified Storage.Queries.Person as PersonQuery
import qualified Storage.Queries.QueriesExtra.RideLite as QRideLite
import qualified Storage.Queries.Translations as MTQuery
import qualified Storage.Queries.Vehicle as QVehicle
import qualified Storage.Queries.VehicleRegistrationCertificateExtra as VRCE
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
        isMandatoryForEnabling = fromMaybe isMandatory isMandatoryForEnabling,
        applicableTo = applicableTo,
        documentFields = documentFields,
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
  cabConfigs <- SDO.filterInCompatibleFlows makeSelfieAadhaarPanMandatory <$> mapM (mkDocumentVerificationConfigAPIEntity personLanguage) (SDO.filterVehicleDocuments cabConfigsRaw mbOnlyVehicle)
  autoConfigs <- SDO.filterInCompatibleFlows makeSelfieAadhaarPanMandatory <$> mapM (mkDocumentVerificationConfigAPIEntity personLanguage) (SDO.filterVehicleDocuments autoConfigsRaw mbOnlyVehicle)
  bikeConfigs <- SDO.filterInCompatibleFlows makeSelfieAadhaarPanMandatory <$> mapM (mkDocumentVerificationConfigAPIEntity personLanguage) (SDO.filterVehicleDocuments bikeConfigsRaw mbOnlyVehicle)
  ambulanceConfigs <- SDO.filterInCompatibleFlows makeSelfieAadhaarPanMandatory <$> mapM (mkDocumentVerificationConfigAPIEntity personLanguage) (SDO.filterVehicleDocuments ambulanceConfigsRaw mbOnlyVehicle)
  truckConfigs <- SDO.filterInCompatibleFlows makeSelfieAadhaarPanMandatory <$> mapM (mkDocumentVerificationConfigAPIEntity personLanguage) (SDO.filterVehicleDocuments truckConfigsRaw mbOnlyVehicle)
  busConfigs <- SDO.filterInCompatibleFlows makeSelfieAadhaarPanMandatory <$> mapM (mkDocumentVerificationConfigAPIEntity personLanguage) (SDO.filterVehicleDocuments busConfigsRaw mbOnlyVehicle)
  boatConfigs <- SDO.filterInCompatibleFlows makeSelfieAadhaarPanMandatory <$> mapM (mkDocumentVerificationConfigAPIEntity personLanguage) (SDO.filterVehicleDocuments boatConfigsRaw mbOnlyVehicle)
  return $
    API.Types.UI.DriverOnboardingV2.DocumentVerificationConfigList
      { cabs = SDO.toMaybe cabConfigs,
        autos = SDO.toMaybe autoConfigs,
        bikes = SDO.toMaybe bikeConfigs,
        ambulances = SDO.toMaybe ambulanceConfigs,
        trucks = SDO.toMaybe truckConfigs,
        bus = SDO.toMaybe busConfigs,
        boat = SDO.toMaybe boatConfigs
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
  distanceUnit <- SMerchant.getDistanceUnitByMerchantOpCity merchantOperatingCityId
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
      mbRateCard <- getRateCardForServiceTier mbDistance mbDuration mbPickup transporterConfig tripCategory distanceUnit serviceTierType
      return $ maybeToList mbRateCard
    Nothing -> do
      rateCards <- mapM (getRateCardForServiceTier mbDistance mbDuration mbPickup transporterConfig tripCategory distanceUnit) driverVehicleServiceTierTypes
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

    getRateCardForServiceTier :: Maybe Meters -> Maybe Minutes -> Maybe LatLong -> Maybe TransporterConfig -> TripCategory -> DistanceUnit -> Domain.Types.Common.ServiceTierType -> Environment.Flow (Maybe API.Types.UI.DriverOnboardingV2.RateCardResp)
    getRateCardForServiceTier mbDistance mbDuration mbPickupLatLon transporterConfig tripCategory distanceUnit serviceTierType = do
      now <- getCurrentTime
      eitherFullFarePolicy <-
        try @_ @SomeException (getFarePolicy mbPickupLatLon Nothing Nothing Nothing Nothing Nothing merchantOperatingCityId False tripCategory serviceTierType Nothing Nothing Nothing Nothing [])
          >>= \case
            Left _ -> try @_ @SomeException $ getFarePolicy Nothing Nothing Nothing Nothing Nothing Nothing merchantOperatingCityId False (Delivery OneWayOnDemandDynamicOffer) serviceTierType Nothing Nothing Nothing Nothing []
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
                  petCharges = Nothing,
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
                  merchantOperatingCityId = Just merchantOperatingCityId,
                  mbAdditonalChargeCategories = Nothing
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
          let rateCardItems = catMaybes $ mkFarePolicyBreakups EulerHS.Prelude.id mkBreakupItem Nothing Nothing Nothing totalFare.amount Nothing (fullFarePolicyToFarePolicy fullFarePolicy)
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
postDriverRegisterPancard (mbPersonId, merchantId, merchantOpCityId) req = postDriverRegisterPancardHelper (mbPersonId, merchantId, merchantOpCityId) False req

postDriverRegisterPancardHelper ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Bool ->
    API.Types.UI.DriverOnboardingV2.DriverPanReq ->
    Flow APISuccess
  )
postDriverRegisterPancardHelper (mbPersonId, merchantId, merchantOpCityId) isDashboard req = do
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
  verificationStatus <- case mbPanVerificationService of
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
      Redis.withLockRedisAndReturnValue (Image.imageS3Lock (imageMetadata.s3Path)) 5 $
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
      Redis.withLockRedisAndReturnValue (Image.imageS3Lock (imageMetadata.s3Path)) 5 $
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
      when (merchantOpCity.country `notElem` [Context.USA, Context.Netherlands]) $ throwError $ InvalidRequest "Bank account creation is only supported for USA and Netherlands"

      mbMobileNumber <- mapM decrypt person.mobileNumber
      mobileNumber <- mbMobileNumber & fromMaybeM (InvalidRequest "Mobile number is required for opening a bank account")

      driverLicense <- runInReplica $ QDL.findByDriverId person.id >>= fromMaybeM (DriverDLNotFound person.id.getId)
      driverDob <- driverLicense.driverDob & fromMaybeM (InvalidRequest "Driver DOB is required for opening a bank account")
      -- idNumber <- decrypt driverLicense.licenseNumber

      ssnLast4 <-
        if merchantOpCity.country == Context.USA
          then do
            driverSSN <- runInReplica $ QDriverSSN.findByDriverId person.id >>= fromMaybeM (DriverSSNNotFound person.id.getId)
            ssnNumber <- decrypt driverSSN.ssn
            return $ Just $ T.takeEnd 4 ssnNumber
          else return Nothing

      let createAccountReq =
            Payment.IndividualConnectAccountReq
              { country = merchantOpCity.country,
                email = person.email,
                dateOfBirth = DT.utctDay driverDob,
                firstName = person.firstName,
                lastName = person.lastName,
                address = Nothing, -- will add later
                ssnLast4 = ssnLast4,
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

----------- DigiLocker Integration Configuration and Helpers -----------

-- TODO: Move these to environment variables in AppCfg/AppEnv
-- For now keeping them as constants, will be updated to fetch from environment
digiLockerClientId :: Text
digiLockerClientId = "YOUR_CLIENT_ID" -- TODO: Fetch from environment

digiLockerClientSecret :: Text
digiLockerClientSecret = "YOUR_CLIENT_SECRET" -- TODO: Fetch from environment

digiLockerRedirectUri :: Text
digiLockerRedirectUri = "YOUR_REDIRECT_URI" -- TODO: Fetch from environment

digiLockerTokenUrl :: Text
digiLockerTokenUrl = "http://digilocker.meripehchaan.gov.in/public/oauth2/1/token"

-- Servant API type for DigiLocker token endpoint
type DigiLockerTokenAPI =
  Servant.ReqBody '[Servant.JSON] APITypes.DigiLockerTokenRequest
    Servant.:> Servant.Post '[Servant.JSON] APITypes.DigiLockerTokenResponse

digiLockerTokenAPI :: Servant.Proxy DigiLockerTokenAPI
digiLockerTokenAPI = Servant.Proxy

-- Redis key for DigiLocker state
mkDigiLockerStateKey :: Text -> Text
mkDigiLockerStateKey stateId = "digilocker:state:" <> stateId

-- Redis key for DigiLocker status (for frontend polling)
mkDigiLockerStatusKey :: Id Domain.Types.Person.Person -> Text
mkDigiLockerStatusKey driverId = "digilockerStatus:" <> driverId.getId

-- Redis key for DigiLocker details
mkDigiLockerDetailsKey :: Id Domain.Types.Person.Person -> Text
mkDigiLockerDetailsKey driverId = "digilocker:details:" <> driverId.getId

----------- DigiLocker Helper Functions -----------

-- Parse DigiLocker scope string to extract document URIs
-- Format: "files.issueddocs issued/in.gov.transport-DRVLC-GJ06001227 userdetails issued/in.gov.transport-RVCER-GJ06J066 picture issued/in.gov.pan-PANCR-F2525N"
parseDigiLockerScope :: Text -> [(Text, Text)] -- Returns list of (type, uri) tuples where type is "issued" or "pull"
parseDigiLockerScope scopeText = do
  let tokens = T.words scopeText
  catMaybes $ map parseToken tokens
  where
    parseToken :: Text -> Maybe (Text, Text)
    parseToken token
      | "issued/" `T.isPrefixOf` token = Just ("issued", token)
      | "file.partners/" `T.isPrefixOf` token = Just ("pull", token)
      | otherwise = Nothing

-- Extract document type from URI
-- Examples:
--   "issued/in.gov.pan-PANCR-F2525N" -> "PAN"
--   "issued/in.gov.transport-DRVLC-GJ06001227" -> "DL"
--   "issued/in.gov.transport-RVCER-GJ06J066" -> "RC"
extractDocumentType :: (Text, Text) -> Text
extractDocumentType (_, uri) =
  case T.splitOn "-" uri of
    (_ : docType : _) -> case T.toUpper docType of
      "PANCR" -> "PAN"
      "DRVLC" -> "DL"
      "RVCER" -> "RC"
      other -> other
    _ -> "UNKNOWN"

-- Main document processing function (runs in background)
processDigiLockerDocuments ::
  Id Domain.Types.Person.Person ->
  Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity ->
  Text -> -- access token
  Text -> -- scope
  Text -> -- details Redis key
  Flow ()
processDigiLockerDocuments driverId merchantOpCityId accessToken scopeText detailsKey = do
  logInfo $ "DigiLocker processing - Starting background processing for DriverId: " <> driverId.getId

  -- Parse scope to get issued and pull documents
  let allDocs = parseDigiLockerScope scopeText
  let issuedDocs = filter (\(docType, _) -> docType == "issued") allDocs
  let pullDocs = filter (\(docType, _) -> docType == "pull") allDocs

  logInfo $ "DigiLocker processing - Found " <> show (length issuedDocs) <> " issued docs and " <> show (length pullDocs) <> " pull docs. DriverId: " <> driverId.getId

  -- Get mandatory documents for this city
  mandatoryDocTypes <- getMandatoryDocumentTypes merchantOpCityId

  -- Process issued documents
  processedIssuedDocs <- mapM (processIssuedDocument driverId merchantOpCityId accessToken mandatoryDocTypes) issuedDocs

  -- Process pull documents (just mark with required fields, don't fetch yet)
  processedPullDocs <- mapM (processPullDocument driverId merchantOpCityId mandatoryDocTypes) pullDocs

  -- Update Redis with final document statuses
  let allProcessedDocs = processedIssuedDocs <> processedPullDocs
  mbCurrentData <- Redis.get detailsKey
  whenJust mbCurrentData $ \currentData -> do
    let updatedData = currentData {APITypes.documents = allProcessedDocs}
    Redis.setExp detailsKey updatedData 3600

  logInfo $ "DigiLocker processing - Completed. Processed " <> show (length allProcessedDocs) <> " documents. DriverId: " <> driverId.getId

-- Get mandatory document types for a city
getMandatoryDocumentTypes :: Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> Flow [Text]
getMandatoryDocumentTypes _merchantOpCityId = do
  -- TODO: This should check DocumentVerificationConfig to determine which docs are mandatory
  -- For now, returning common mandatory documents
  return ["DL", "RC", "PAN"]

-- Process a single issued document
processIssuedDocument ::
  Id Domain.Types.Person.Person ->
  Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity ->
  Text -> -- access token
  [Text] -> -- mandatory doc types
  (Text, Text) -> -- (type, uri)
  Flow APITypes.DigiLockerDocumentStatus
processIssuedDocument driverId merchantOpCityId accessToken mandatoryDocTypes (_, uri) = do
  let docType = extractDocumentType ("issued", uri)

  -- Check if this document type is mandatory
  let isMandatory = docType `elem` mandatoryDocTypes

  -- Check if document already exists in DB
  alreadyExists <- checkDocumentExistsInDB driverId docType

  if not isMandatory || alreadyExists
    then do
      -- Not mandatory or already exists - skip processing
      logDebug $ "DigiLocker processing - Skipping " <> docType <> " (mandatory: " <> show isMandatory <> ", exists: " <> show alreadyExists <> "). DriverId: " <> driverId.getId
      return
        APITypes.DigiLockerDocumentStatus
          { documentType = docType,
            status = if alreadyExists then "VALID" else "SKIPPED",
            availability = "ISSUED",
            pullFields = Nothing,
            errorCode = Nothing,
            errorDescription = Nothing
          }
    else do
      -- Fetch and process the document
      logInfo $ "DigiLocker processing - Fetching " <> docType <> " from DigiLocker. DriverId: " <> driverId.getId
      result <- try @_ @SomeException (fetchAndStoreDocument driverId merchantOpCityId accessToken uri docType)
      case result of
        Left err -> do
          logError $ "DigiLocker processing - Failed to fetch " <> docType <> ". Error: " <> show err <> ", DriverId: " <> driverId.getId
          return
            APITypes.DigiLockerDocumentStatus
              { documentType = docType,
                status = "FAILED",
                availability = "ISSUED",
                pullFields = Nothing,
                errorCode = Just "FETCH_FAILED",
                errorDescription = Just $ T.pack $ show err
              }
        Right () -> do
          logInfo $ "DigiLocker processing - Successfully processed " <> docType <> ". DriverId: " <> driverId.getId
          return
            APITypes.DigiLockerDocumentStatus
              { documentType = docType,
                status = "VALID",
                availability = "ISSUED",
                pullFields = Nothing,
                errorCode = Nothing,
                errorDescription = Nothing
              }

-- Process a pull document (mark with required fields)
processPullDocument ::
  Id Domain.Types.Person.Person ->
  Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity ->
  [Text] -> -- mandatory doc types
  (Text, Text) -> -- (type, uri)
  Flow APITypes.DigiLockerDocumentStatus
processPullDocument driverId _merchantOpCityId mandatoryDocTypes (_, uri) = do
  let docType = extractDocumentType ("pull", uri)

  -- Check if document already exists in DB
  alreadyExists <- checkDocumentExistsInDB driverId docType

  -- Check if this document type is mandatory
  let isMandatory = docType `elem` mandatoryDocTypes

  if alreadyExists || not isMandatory
    then do
      -- Already exists or not mandatory - skip
      logDebug $ "DigiLocker processing - Skipping pull doc " <> docType <> " (mandatory: " <> show isMandatory <> ", exists: " <> show alreadyExists <> "). DriverId: " <> driverId.getId
      return
        APITypes.DigiLockerDocumentStatus
          { documentType = docType,
            status = if alreadyExists then "VALID" else "SKIPPED",
            availability = "ISSUED",
            pullFields = Nothing,
            errorCode = Nothing,
            errorDescription = Nothing
          }
    else do
      -- Needs manual entry to pull
      let requiredFields = getPullRequiredFields docType
      logInfo $ "DigiLocker processing - Marking " <> docType <> " as PULL with fields: " <> T.intercalate ", " requiredFields <> ". DriverId: " <> driverId.getId
      return
        APITypes.DigiLockerDocumentStatus
          { documentType = docType,
            status = "PENDING",
            availability = "PULL",
            pullFields = Just requiredFields,
            errorCode = Nothing,
            errorDescription = Nothing
          }

-- Get required fields for pulling a document
getPullRequiredFields :: Text -> [Text]
getPullRequiredFields docType = case docType of
  "PAN" -> ["DOB", "PAN_Number"]
  "DL" -> ["DOB", "DL_Number"]
  "RC" -> ["RC_Number"]
  _ -> []

-- Check if document already exists in DB
checkDocumentExistsInDB :: Id Domain.Types.Person.Person -> Text -> Flow Bool
checkDocumentExistsInDB driverId docType = do
  case docType of
    "PAN" -> do
      mbPan <- QDPC.findByDriverId driverId
      return $ isJust mbPan && maybe False (\pan -> pan.verificationStatus == Documents.VALID) mbPan
    "DL" -> do
      mbDL <- QDL.findByDriverId driverId
      return $ isJust mbDL && maybe False (\dl -> dl.verificationStatus == Documents.VALID) mbDL
    "RC" -> do
      rcList <- DRAE.findAllByDriverId driverId
      let mbRC = fmap snd $ listToMaybe rcList
      return $ isJust mbRC && maybe False (\rc -> rc.verificationStatus == Documents.VALID) mbRC
    _ -> return False

-- Fetch document from DigiLocker and store in DB
fetchAndStoreDocument ::
  Id Domain.Types.Person.Person ->
  Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity ->
  Text -> -- access token
  Text -> -- document URI (e.g., "issued/in.gov.pan-PANCR-F2525N")
  Text -> -- document type
  Flow ()
fetchAndStoreDocument driverId _merchantOpCityId _accessToken uri docType = do
  -- Construct DigiLocker XML API URL
  let xmlUrl = "https://digilocker.meripehchaan.gov.in/public/oauth2/1/xml/" <> T.replace "issued/" "" uri

  logInfo $ "DigiLocker processing - Fetching document from: " <> xmlUrl <> ", DriverId: " <> driverId.getId

  -- TODO: Implement actual HTTP call to DigiLocker XML API with access token
  -- TODO: Parse XML response
  -- TODO: Extract document data based on docType
  -- TODO: Store in appropriate DB table (PAN, DL, RC, etc.)
  -- For now, just logging that we would do this
  logInfo $ "DigiLocker processing - TODO: Fetch and store " <> docType <> " document. DriverId: " <> driverId.getId

  -- This will be implemented in the next iteration with actual XML parsing and DB updates
  return ()

postDobppVerifyCallbackDigiLocker ::
  ( Kernel.Prelude.Maybe Kernel.Prelude.Text ->
    Kernel.Prelude.Maybe Kernel.Prelude.Text ->
    Kernel.Prelude.Text ->
    Kernel.Prelude.Text ->
    Environment.Flow APISuccess
  )
postDobppVerifyCallbackDigiLocker mbError mbErrorDescription code stateParam = do
  -- Step 1: Validate code and state parameters
  -- If either is empty, log and return error (500 Internal Error)
  when (T.null code || T.null stateParam) $ do
    logError $ "DigiLocker callback - Missing required parameters. Code empty: " <> show (T.null code) <> ", State empty: " <> show (T.null stateParam)
    throwError $ InternalError "DigiLocker callback received with empty code or state parameter"

  -- Step 2: Fetch driver information from Redis using state
  let stateKey = mkDigiLockerStateKey stateParam
  logDebug $ "DigiLocker callback - Fetching state data from Redis key: " <> stateKey
  mbStateData <- Redis.get stateKey :: Flow (Maybe APITypes.DigiLockerStateData)
  stateData <- mbStateData & fromMaybeM (InvalidRequest "Invalid or expired state parameter from DigiLocker")

  let driverId = APITypes.driverId stateData
  let codeVerifier = APITypes.codeVerifier stateData
  let detailsKey = mkDigiLockerDetailsKey driverId

  logInfo $ "DigiLocker callback - Received callback for DriverId: " <> driverId.getId <> ", State: " <> stateParam

  -- Handle error cases from DigiLocker
  whenJust mbError $ \errorCode -> do
    let errorMsg = fromMaybe "Unknown DigiLocker error" mbErrorDescription
    logError $ "DigiLocker callback - Error from DigiLocker. Code: " <> errorCode <> ", Description: " <> errorMsg <> ", DriverId: " <> driverId.getId

    -- Store error in details Redis key
    let errorDetailsData =
          APITypes.DigiLockerDetailsData
            { digilocker =
                APITypes.DigiLockerAuthData
                  { stateID = Just False,
                    codeID = Just False,
                    errorCode = Just errorCode,
                    errorDescription = Just errorMsg,
                    accessToken = Nothing,
                    scope = Nothing
                  },
              documents = []
            }
    Redis.setExp detailsKey errorDetailsData 3600 -- 1 hour TTL
    throwError $ InvalidRequest $ "DigiLocker authentication failed: " <> errorCode

  -- Step 3: Fetch person and merchant to validate merchant
  person <- runInReplica $ PersonQuery.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  merchant <- CQM.findById person.merchantId >>= fromMaybeM (MerchantDoesNotExist person.merchantId.getId)

  -- Step 4: Check if merchant is Namma Yatri
  unless (merchant.name == "Namma Yatri") $ do
    logError $ "DigiLocker callback - Merchant mismatch. Expected: Namma Yatri, Got: " <> merchant.name <> ", DriverId: " <> driverId.getId
    throwError $ InvalidRequest "DigiLocker verification is only supported for Namma Yatri merchant"

  logInfo $ "DigiLocker callback - Merchant validated: " <> merchant.name <> ", DriverId: " <> driverId.getId

  -- Step 5: Call DigiLocker token API to exchange code for access token
  let tokenRequest =
        APITypes.DigiLockerTokenRequest
          { grant_type = "authorization_code",
            code = code,
            client_id = digiLockerClientId,
            client_secret = digiLockerClientSecret,
            redirect_uri = digiLockerRedirectUri,
            code_verifier = codeVerifier
          }

  logInfo $ "DigiLocker callback - Calling token API for DriverId: " <> driverId.getId

  -- Make the HTTP call to DigiLocker token endpoint
  tokenUrl <- Kernel.Prelude.parseBaseUrl digiLockerTokenUrl
  let eulerClient = Euler.client digiLockerTokenAPI tokenRequest
  tokenResponse <-
    ( try @_ @SomeException (callAPI tokenUrl eulerClient "digilocker-token" digiLockerTokenAPI)
        >>= \case
          Left err -> do
            logError $ "DigiLocker callback - Token API call failed. Error: " <> show err <> ", DriverId: " <> driverId.getId
            -- Store error in details Redis key
            let errorDetailsData =
                  APITypes.DigiLockerDetailsData
                    { digilocker =
                        APITypes.DigiLockerAuthData
                          { stateID = Just True,
                            codeID = Just False,
                            errorCode = Just "TOKEN_API_FAILED",
                            errorDescription = Just $ T.pack $ show err,
                            accessToken = Nothing,
                            scope = Nothing
                          },
                      documents = []
                    }
            Redis.setExp detailsKey errorDetailsData 3600
            throwError $ InternalError "Failed to obtain access token from DigiLocker API"
          Right eitherResp -> case eitherResp of
            Left clientErr -> do
              logError $ "DigiLocker callback - Token API client error: " <> show clientErr <> ", DriverId: " <> driverId.getId
              throwError $ InternalError "DigiLocker token API returned client error"
            Right resp -> return resp
    ) ::
      Flow APITypes.DigiLockerTokenResponse

  logInfo $ "DigiLocker callback - Token API success. DriverId: " <> driverId.getId

  -- Step 6: Parse scope to identify documents and mark all as PENDING initially
  let scopeText = case tokenResponse of
        APITypes.DigiLockerTokenResponse {APITypes.scope = s} -> fromMaybe "" s
  let documentURIs = parseDigiLockerScope scopeText
  let initialDocuments =
        map
          ( \uri ->
              APITypes.DigiLockerDocumentStatus
                { documentType = extractDocumentType uri,
                  status = "PENDING",
                  availability = "UNKNOWN",
                  pullFields = Nothing,
                  errorCode = Nothing,
                  errorDescription = Nothing
                }
          )
          documentURIs

  -- Store access token, scope, and pending documents in Redis
  let pendingDetailsData =
        APITypes.DigiLockerDetailsData
          { digilocker =
              APITypes.DigiLockerAuthData
                { stateID = Just True,
                  codeID = Just True,
                  errorCode = Nothing,
                  errorDescription = Nothing,
                  accessToken = Just (case tokenResponse of APITypes.DigiLockerTokenResponse {APITypes.access_token = at} -> at),
                  scope = case tokenResponse of APITypes.DigiLockerTokenResponse {APITypes.scope = s} -> s
                },
            documents = initialDocuments
          }
  Redis.setExp detailsKey pendingDetailsData 3600 -- 1 hour TTL
  logInfo $ "DigiLocker callback - Access token stored, marked " <> show (length initialDocuments) <> " documents as PENDING. DriverId: " <> driverId.getId

  -- Step 7: Return success immediately and continue processing in background thread
  fork "digilocker-document-processing" $ do
    let accessToken = case tokenResponse of APITypes.DigiLockerTokenResponse {APITypes.access_token = at} -> at
    processDigiLockerDocuments driverId person.merchantOperatingCityId accessToken scopeText detailsKey
      `catchAny` \err -> do
        logError $ "DigiLocker document processing failed - Error: " <> show err <> ", DriverId: " <> driverId.getId
        -- Update Redis with error
        mbCurrentData <- Redis.get detailsKey
        whenJust mbCurrentData $ \currentData -> do
          let currentDigiLocker = APITypes.digilocker currentData
          let updatedDigiLocker = currentDigiLocker {APITypes.errorCode = Just "PROCESSING_FAILED", APITypes.errorDescription = Just $ T.pack $ show err} :: APITypes.DigiLockerAuthData
          let errorData = currentData {APITypes.digilocker = updatedDigiLocker}
          Redis.setExp detailsKey errorData 3600

  return Success
