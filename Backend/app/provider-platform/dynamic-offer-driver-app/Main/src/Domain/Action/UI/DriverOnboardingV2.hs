module Domain.Action.UI.DriverOnboardingV2 where

import qualified API.Types.UI.DriverOnboardingV2
import qualified API.Types.UI.DriverOnboardingV2 as APITypes
import qualified AWS.S3 as S3
import qualified Control.Monad.Extra as CME
import qualified Crypto.Hash as Hash
import Crypto.Random (getRandomBytes)
import Data.Aeson (Value (..), object)
import qualified Data.Aeson.KeyMap as HM
import qualified Data.ByteArray as BA
import qualified Data.ByteString.Base64 as B64
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (defaultTimeLocale, formatTime)
import qualified Data.Time as DT
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import qualified Domain.Action.UI.DriverOnboarding.Image as Image
import qualified Domain.Action.UI.DriverOnboarding.PullDocument as PullDocument
import qualified Domain.Types.AadhaarCard
import Domain.Types.BackgroundVerification
import Domain.Types.Common
import qualified Domain.Types.CommonDriverOnboardingDocuments
import qualified Domain.Types.DigilockerVerification as DDV
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
import qualified Domain.Types.MerchantServiceConfig as DMSC
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
import qualified Kernel.External.Payment.Interface as Payment
import Kernel.External.Types (Language (..), ServiceFlow)
import qualified Kernel.External.Verification.Digilocker.Types as DigilockerTypes
import qualified Kernel.External.Verification.Interface as VI
import qualified Kernel.External.Verification.Interface.Types as Verification
import qualified Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Beckn.DecimalValue as DecimalValue
import qualified Kernel.Types.Documents as Documents
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Network.HTTP.Types.URI as URI
import SharedLogic.DriverOnboarding
import qualified SharedLogic.DriverOnboarding as SDO
import SharedLogic.FareCalculator
import SharedLogic.FarePolicy
import qualified SharedLogic.Merchant as SMerchant
import SharedLogic.VehicleServiceTier
import qualified Storage.Cac.MerchantServiceUsageConfig as CQMSUC
import qualified Storage.Cac.TransporterConfig as CQTC
import qualified Storage.CachedQueries.DocumentVerificationConfig as CQDVC
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as CQMSC
import qualified Storage.CachedQueries.VehicleServiceTier as CQVST
import qualified Storage.Queries.AadhaarCard as QAadhaarCard
import qualified Storage.Queries.BackgroundVerification as QBV
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.CommonDriverOnboardingDocuments as QCommonDriverOnboardingDocuments
import qualified Storage.Queries.DigilockerVerification as QDV
import qualified Storage.Queries.DriverBankAccount as QDBA
import qualified Storage.Queries.DriverGstin as QDGTIN
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.DriverLicense as QDL
import qualified Storage.Queries.DriverPanCard as QDPC
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
  verificationStatus <-
    if req.verifiedBy == Just DPC.DIGILOCKER
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

-- Helper function to create DigiLocker logs
----------- DigiLocker Integration Configuration and Helpers -----------

-- DigiLocker Configuration Type will be auto-generated from API YAML spec
-- Import: API.Types.UI.DriverOnboardingV2.DigiLockerCfg (after running generator)

-- DigiLocker config is fetched from MerchantServiceConfig (credentials, URLs)
-- TransporterConfig.digilockerEnabled determines if feature is enabled

getDigiLockerConfig :: Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> Flow DigilockerTypes.DigiLockerCfg
getDigiLockerConfig merchantOpCityId = do
  -- Check if DigiLocker is enabled for this merchant+city
  transporterConfig <-
    CQTC.findByMerchantOpCityId merchantOpCityId Nothing
      >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)

  unless (transporterConfig.digilockerEnabled == Just True) $
    throwError $ InvalidRequest "DigiLocker not enabled for this merchant"

  -- Fetch DigiLocker service config
  let serviceName = DMSC.VerificationService Verification.DigiLocker
  merchantServiceConfig <-
    CQMSC.findByServiceAndCity serviceName merchantOpCityId
      >>= fromMaybeM (InternalError "DigiLocker service config not found. Please configure DigiLocker in merchant_service_config table.")

  case merchantServiceConfig.serviceConfig of
    DMSC.VerificationServiceConfig (Verification.DigiLockerConfig config) ->
      return config
    _ -> throwError $ InternalError "Invalid DigiLocker service config type"

----------- GET DIGILOCKER AUTHORIZATION URL -----------

postDriverDigilockerInitiate ::
  ( Maybe (Id Domain.Types.Person.Person),
    Id Domain.Types.Merchant.Merchant,
    Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
  ) ->
  APITypes.DigiLockerInitiateReq -> -- Added request parameter
  Environment.Flow APITypes.DigiLockerInitiateResp
postDriverDigilockerInitiate (mbDriverId, merchantId, merchantOpCityId) req = do
  driverId <- mbDriverId & fromMaybeM (PersonNotFound "No person found")
  logInfo $ "DigiLocker initiate - Starting authorization flow for DriverId: " <> driverId.getId <> ", VehicleCategory: " <> show req.vehicleCategory

  -- Step 1: Verify DigiLocker is enabled for this merchant+city
  verifyDigiLockerEnabled merchantOpCityId

  -- Step 2: Check for existing active session
  latestSession <- QDV.findLatestByDriverId (Just 1) (Just 0) driverId

  case latestSession of
    [] -> do
      -- No existing session - create new one
      logInfo $ "DigiLocker initiate - No existing session found for DriverId: " <> driverId.getId
      createNewDigiLockerSession driverId merchantId merchantOpCityId req.vehicleCategory
    (session : _) -> do
      -- Existing session found - validate and decide action
      logInfo $ "DigiLocker initiate - Found existing session for DriverId: " <> driverId.getId <> ", SessionStatus: " <> show session.sessionStatus
      handleExistingSession driverId merchantId merchantOpCityId session req.vehicleCategory

----------- Helper Functions for DigiLocker Initiate -----------

-- Verify DigiLocker is enabled for merchant+city
verifyDigiLockerEnabled :: Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> Flow ()
verifyDigiLockerEnabled merchantOpCityId = do
  transporterConfig <-
    CQTC.findByMerchantOpCityId merchantOpCityId Nothing
      >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)

  -- Check if digilockerEnabled is true in TransporterConfig
  unless (fromMaybe False transporterConfig.digilockerEnabled) $
    throwError $ InvalidRequest "DigiLocker is not enabled for this merchant+city"

  logInfo $ "DigiLocker initiate - Verified DigiLocker is enabled for merchantOpCityId: " <> merchantOpCityId.getId

-- Handle existing session logic
handleExistingSession ::
  Id Domain.Types.Person.Person ->
  Id Domain.Types.Merchant.Merchant ->
  Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity ->
  DDV.DigilockerVerification ->
  DVC.VehicleCategory -> -- Vehicle category for new session if needed
  Flow APITypes.DigiLockerInitiateResp
handleExistingSession driverId merchantId merchantOpCityId session vehicleCategory = do
  now <- getCurrentTime

  case session.sessionStatus of
    DDV.PENDING -> handlePendingSession driverId merchantId merchantOpCityId session now
    DDV.SUCCESS -> handleSuccessSession driverId merchantId merchantOpCityId session now vehicleCategory
    DDV.FAILED -> do
      logInfo $ "DigiLocker initiate - Previous session FAILED, creating new session for DriverId: " <> driverId.getId
      createNewDigiLockerSession driverId merchantId merchantOpCityId vehicleCategory
    DDV.CONSENT_DENIED -> do
      logInfo $ "DigiLocker initiate - Previous session CONSENT_DENIED, creating new session for DriverId: " <> driverId.getId
      createNewDigiLockerSession driverId merchantId merchantOpCityId vehicleCategory

-- Handle PENDING session
handlePendingSession ::
  Id Domain.Types.Person.Person ->
  Id Domain.Types.Merchant.Merchant ->
  Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity ->
  DDV.DigilockerVerification ->
  UTCTime ->
  Flow APITypes.DigiLockerInitiateResp
handlePendingSession _driverId _merchantId _merchantOpCityId _session _now = do
  -- Session is PENDING - callback hasn't been called yet
  logInfo $ "DigiLocker initiate - Session PENDING, returning 409"
  throwError DigiLockerVerificationInProgress

-- Handle SUCCESS session
handleSuccessSession ::
  Id Domain.Types.Person.Person ->
  Id Domain.Types.Merchant.Merchant ->
  Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity ->
  DDV.DigilockerVerification ->
  UTCTime ->
  DVC.VehicleCategory -> -- Vehicle category for new session if needed
  Flow APITypes.DigiLockerInitiateResp
handleSuccessSession driverId merchantId merchantOpCityId session now vehicleCategory = do
  -- Check document statuses in docStatus JSON
  checkDocumentStatuses driverId merchantId merchantOpCityId session now vehicleCategory

-- Check document statuses and decide action
checkDocumentStatuses ::
  Id Domain.Types.Person.Person ->
  Id Domain.Types.Merchant.Merchant ->
  Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity ->
  DDV.DigilockerVerification ->
  UTCTime ->
  DVC.VehicleCategory -> -- Vehicle category for new session if needed
  Flow APITypes.DigiLockerInitiateResp
checkDocumentStatuses driverId merchantId merchantOpCityId session now vehicleCategory = do
  -- docStatus is already a Value type, no parsing needed
  let docStatusMap = session.docStatus

  -- Check for PENDING documents
  when (hasDocWithStatus docStatusMap "PENDING") $ do
    logInfo $ "DigiLocker initiate - Found PENDING documents, returning 409"
    throwError DigiLockerDocumentsBeingVerified

  -- Check for FAILED or CONSENT_DENIED documents
  if hasDocWithStatus docStatusMap "FAILED" || hasDocWithStatus docStatusMap "CONSENT_DENIED"
    then do
      logInfo $ "DigiLocker initiate - Found FAILED or CONSENT_DENIED documents, creating new session"
      createNewDigiLockerSession driverId merchantId merchantOpCityId vehicleCategory
    else -- Check for PULL_REQUIRED documents

      if hasDocWithStatus docStatusMap "PULL_REQUIRED"
        then handlePullRequiredDocs driverId merchantId merchantOpCityId session docStatusMap now vehicleCategory
        else -- If no problematic status, check actual document tables
          checkActualDocumentTables driverId merchantId merchantOpCityId vehicleCategory

-- Handle PULL_REQUIRED documents
handlePullRequiredDocs ::
  Id Domain.Types.Person.Person ->
  Id Domain.Types.Merchant.Merchant ->
  Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity ->
  DDV.DigilockerVerification ->
  Value ->
  UTCTime ->
  DVC.VehicleCategory -> -- Vehicle category for new session if needed
  Flow APITypes.DigiLockerInitiateResp
handlePullRequiredDocs driverId merchantId merchantOpCityId session _docStatusMap now vehicleCategory = do
  case session.accessTokenExpiresAt of
    Nothing -> do
      logInfo $ "DigiLocker initiate - PULL_REQUIRED with no expiry, creating new session"
      createNewDigiLockerSession driverId merchantId merchantOpCityId vehicleCategory
    Just expiresAt ->
      if now < expiresAt
        then do
          logInfo $ "DigiLocker initiate - PULL_REQUIRED with valid token, returning 409"
          throwError DigiLockerPullRequired
        else do
          logInfo $ "DigiLocker initiate - PULL_REQUIRED with expired token, creating new session"
          createNewDigiLockerSession driverId merchantId merchantOpCityId vehicleCategory

-- Check actual document tables for verification status
checkActualDocumentTables ::
  Id Domain.Types.Person.Person ->
  Id Domain.Types.Merchant.Merchant ->
  Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity ->
  DVC.VehicleCategory -> -- Vehicle category for new session if needed
  Flow APITypes.DigiLockerInitiateResp
checkActualDocumentTables driverId merchantId merchantOpCityId vehicleCategory = do
  -- Check DriverLicense
  mbDL <- QDL.findByDriverId driverId
  let dlStatus = mbDL >>= (\dl -> Just dl.verificationStatus)

  -- Check PanCard
  mbPan <- QDPC.findByDriverId driverId
  let panStatus = mbPan >>= (\pan -> Just pan.verificationStatus)

  -- Check AadhaarCard
  mbAadhaar <- QAadhaarCard.findByPrimaryKey driverId
  let aadhaarStatus = mbAadhaar >>= (\aadhaar -> Just aadhaar.verificationStatus)

  let allStatuses = [dlStatus, panStatus, aadhaarStatus]

  -- If any document is PENDING → Return 409
  when (any (== Just Documents.PENDING) allStatuses) $ do
    logInfo $ "DigiLocker initiate - Found PENDING documents in tables, returning 409"
    throwError DigiLockerDocumentsBeingVerified

  -- If all documents are VALID → Return error (already verified)
  let allDocumentsValid = all (== Just Documents.VALID) allStatuses
  if allDocumentsValid
    then do
      logInfo $ "DigiLocker initiate - All documents already verified"
      throwError $ InvalidRequest "All documents are already verified. You should not be calling this API."
    else do
      -- Else → Create new session (handles FAILED, INVALID, missing docs, etc.)
      logInfo $ "DigiLocker initiate - Creating new session for document verification"
      createNewDigiLockerSession driverId merchantId merchantOpCityId vehicleCategory

-- Create new DigiLocker session
createNewDigiLockerSession ::
  Id Domain.Types.Person.Person ->
  Id Domain.Types.Merchant.Merchant ->
  Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity ->
  DVC.VehicleCategory -> -- Vehicle category for DL validation
  Flow APITypes.DigiLockerInitiateResp
createNewDigiLockerSession driverId merchantId merchantOpCityId vehicleCategory = do
  logInfo $ "DigiLocker initiate - Creating new session for DriverId: " <> driverId.getId <> ", VehicleCategory: " <> show vehicleCategory

  -- Fetch DigiLocker credentials from MerchantServiceConfig
  digiLockerConfig <- getDigiLockerConfig merchantOpCityId
  logInfo $ "DigiLocker initiate - Config retrieved for merchantOpCityId: " <> merchantOpCityId.getId

  -- Generate PKCE parameters
  randomBytes <- liftIO $ getRandomBytes 24
  now <- getCurrentTime
  let timestampMillis = T.pack $ show $ (floor (utcTimeToPOSIXSeconds now * 1000) :: Integer)
  let timestampBytes = TE.encodeUtf8 timestampMillis
  let combinedBytes = randomBytes <> timestampBytes
  let codeVerifier = base64UrlEncodeNoPadding combinedBytes
  let codeChallenge = generateCodeChallenge codeVerifier
  let codeMethod = "S256"

  -- Generate state ID
  stateId <- generateGUID

  -- Create session record in DB with vehicleCategory
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
            docStatus = object [], -- Empty JSON object
            sessionStatus = DDV.PENDING,
            responseCode = Nothing,
            responseDescription = Nothing,
            tokenResponse = Nothing,
            vehicleCategory = vehicleCategory, -- Store vehicle category for DL validation
            merchantId = Just merchantId,
            merchantOperatingCityId = Just merchantOpCityId,
            createdAt = now,
            updatedAt = now
          }

  QDV.create newSession

  logInfo $ "DigiLocker initiate - Created session with ID: " <> sessionId.getId <> ", StateId: " <> stateId <> ", VehicleCategory: " <> show vehicleCategory

  -- Construct authorization URL with config from MerchantServiceConfig
  let authUrl = constructDigiLockerAuthUrl digiLockerConfig stateId codeChallenge

  return $ APITypes.DigiLockerInitiateResp {authorizationUrl = authUrl}

----------- Helper Functions for JSON Parsing -----------

-- Check if any document has a specific status in the docStatus Value
hasDocWithStatus :: Value -> Text -> Bool
hasDocWithStatus (Object obj) targetStatus =
  any (hasStatus targetStatus) (HM.elems obj)
  where
    hasStatus :: Text -> Value -> Bool
    hasStatus target (Object docObj) =
      case HM.lookup "status" docObj of
        Just (String status) -> status == target
        _ -> False
    hasStatus _ _ = False
hasDocWithStatus _ _ = False

-- Helper: Base64URL encode without padding (as per RFC 7636)
-- Implements: base64url_encode_without_padding
base64UrlEncodeNoPadding :: ByteString -> Text
base64UrlEncodeNoPadding bytes =
  let base64Encoded = B64.encode bytes
      base64Text = TE.decodeUtf8 base64Encoded
      -- Convert Base64 to Base64URL: replace + with -, / with _, and remove padding =
      base64UrlText = T.replace "+" "-" $ T.replace "/" "_" $ T.replace "=" "" base64Text
   in base64UrlText

-- Helper: Generate code_challenge from code_verifier using SHA256 and Base64URL encoding
-- Implements: code_challenge = base64_url_encode_without_padding(sha256(code_verifier))
generateCodeChallenge :: Text -> Text
generateCodeChallenge codeVerifier =
  let verifierBytes = TE.encodeUtf8 codeVerifier
      digest = Hash.hashWith Hash.SHA256 verifierBytes
      hashBytes = BA.convert digest :: ByteString
   in base64UrlEncodeNoPadding hashBytes

-- Helper: Construct DigiLocker authorization URL with all required parameters
constructDigiLockerAuthUrl :: DigilockerTypes.DigiLockerCfg -> Text -> Text -> Text
constructDigiLockerAuthUrl config digiLockerState codeChallenge =
  let baseUrl = Kernel.Prelude.showBaseUrl config.url
      authPath = "/public/oauth2/1/authorize"
      params =
        [ ("response_type", "code"),
          ("client_id", config.clientId),
          ("redirect_uri", config.redirectUri),
          ("state", digiLockerState),
          ("code_challenge", codeChallenge),
          ("code_challenge_method", config.codeChallengeMethod),
          ("pla", "Y"),
          ("plsignup", "Y"),
          ("ulsignup", "Y"),
          ("purpose", "verification")
        ]
      queryString = T.intercalate "&" $ map (\(k, v) -> k <> "=" <> encodeURIComponent v) params
   in baseUrl <> authPath <> "?" <> queryString
  where
    -- URL encode text for query parameters
    encodeURIComponent :: Text -> Text
    encodeURIComponent txt =
      TE.decodeUtf8 $ URI.urlEncode True $ TE.encodeUtf8 txt

----------- PULL DOCUMENTS FROM DIGILOCKER -----------

-- | Pull driving license document from DigiLocker
-- This endpoint is called when user submits DL details within 1 hour of starting DigiLocker session
postDriverDigilockerPullDocuments ::
  ( Maybe (Id Domain.Types.Person.Person),
    Id Domain.Types.Merchant.Merchant,
    Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
  ) ->
  APITypes.PullDrivingLicenseReq ->
  Environment.Flow APISuccess
postDriverDigilockerPullDocuments (mbDriverId, merchantId, merchantOpCityId) req = do
  logInfo $ "PullDocuments - Starting pull operation for DocType: " <> show req.docType
  PullDocument.pullDrivingLicenseDocument (mbDriverId, merchantId, merchantOpCityId) req
