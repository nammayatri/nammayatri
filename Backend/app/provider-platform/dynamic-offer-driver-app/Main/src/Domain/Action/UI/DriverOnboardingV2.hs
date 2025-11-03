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
import Data.OpenApi (ToSchema)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import Data.Time (defaultTimeLocale, formatTime, parseTimeM)
import qualified Data.Time as DT
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import qualified Domain.Action.UI.DriverOnboarding.Image as Image
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
import qualified Domain.Types.DriverLicense
import qualified Domain.Types.DriverLicense as DDL
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
import qualified EulerHS.Types as Euler
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
import Kernel.Utils.JSON (stripPrefixUnderscoreIfAny)
import qualified Network.HTTP.Types.URI as URI
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
import qualified Storage.Queries.DriverRCAssociationExtra as DRAE
import qualified Storage.Queries.DriverSSN as QDriverSSN
import qualified Storage.Queries.HyperVergeSdkLogs as HVSdkLogsQuery
import qualified Storage.Queries.Image as ImageQuery
import qualified Storage.Queries.Person as PersonQuery
import qualified Storage.Queries.QueriesExtra.RideLite as QRideLite
import qualified Storage.Queries.Translations as MTQuery
import qualified Storage.Queries.Vehicle as QVehicle
import qualified Storage.Queries.VehicleRegistrationCertificateExtra as VRCE
import qualified Text.XML as XML
import qualified Text.XML.Cursor as XML
import qualified Tools.BackgroundVerification as BackgroundVerificationT
import Tools.Error
import qualified Tools.Payment as TPayment
import qualified Tools.Verification as Verification
import Utils.Common.Cac.KeyNameConstants

-- DigiLockerPullDocResponse with custom JSON handling
-- The DigiLocker API returns "error" (without underscore) in JSON
data DigiLockerPullDocResponse = DigiLockerPullDocResponse
  { uri :: Maybe Text,
    _error :: Maybe Text,
    error_description :: Maybe Text
  }
  deriving (Generic, Show, ToSchema)

instance FromJSON DigiLockerPullDocResponse where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON DigiLockerPullDocResponse where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

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

-- Servant API type for DigiLocker token endpoint
type DigiLockerTokenAPI =
  Servant.ReqBody '[Servant.JSON] APITypes.DigiLockerTokenRequest
    Servant.:> Servant.Post '[Servant.JSON] APITypes.DigiLockerTokenResponse

digiLockerTokenAPI :: Servant.Proxy DigiLockerTokenAPI
digiLockerTokenAPI = Servant.Proxy

-- Servant API type for DigiLocker file/xml fetch endpoint
-- DigiLocker returns raw binary data (PDF) or XML text
type DigiLockerFileAPI =
  Servant.Header "Authorization" Text
    Servant.:> Servant.Get '[Servant.PlainText] Text

digiLockerFileAPI :: Servant.Proxy DigiLockerFileAPI
digiLockerFileAPI = Servant.Proxy

-- Servant API type for DigiLocker pull document endpoint
type DigiLockerPullDocAPI =
  Servant.Header "Authorization" Text
    Servant.:> Servant.Header "Content-Type" Text
    Servant.:> Servant.ReqBody '[Servant.FormUrlEncoded] [(Text, Text)]
    Servant.:> Servant.Post '[Servant.JSON] DigiLockerPullDocResponse

digiLockerPullDocAPI :: Servant.Proxy DigiLockerPullDocAPI
digiLockerPullDocAPI = Servant.Proxy

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

    -- Log CALLBACK flow (error case)
    -- TODO: Add DigiLocker logging
    -- createDigiLockerLog removed - will be replaced with new logging approach

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

  -- Step 5: Fetch DigiLocker config
  digiLockerConfig <- getDigiLockerConfig person.merchantOperatingCityId
  logInfo $ "DigiLocker callback - Config retrieved for merchantOpCityId: " <> person.merchantOperatingCityId.getId

  -- Step 6: Call DigiLocker token API to exchange code for access token
  let tokenRequest =
        APITypes.DigiLockerTokenRequest
          { grant_type = "authorization_code",
            code = code,
            client_id = digiLockerConfig.clientId,
            client_secret = digiLockerConfig.clientSecret,
            redirect_uri = digiLockerConfig.redirectUri,
            code_verifier = codeVerifier
          }

  logInfo $ "DigiLocker callback - Calling token API for DriverId: " <> driverId.getId

  -- Make the HTTP call to DigiLocker token endpoint
  let tokenUrlParsed = digiLockerConfig.url
  let eulerClient = Euler.client digiLockerTokenAPI tokenRequest
  tokenResponse <-
    ( try @_ @SomeException (callAPI tokenUrlParsed eulerClient "digilocker-token" digiLockerTokenAPI)
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

  -- Log TOKEN_EXCHANGE flow
  -- TODO: Add DigiLocker logging
  -- createDigiLockerLog removed - will be replaced with new logging approach

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
          let updatedDigiLocker = (currentDigiLocker :: APITypes.DigiLockerAuthData) {APITypes.errorCode = Just "PROCESSING_FAILED", APITypes.errorDescription = Just $ T.pack $ show err}
          let errorData = currentData {APITypes.digilocker = updatedDigiLocker}
          Redis.setExp detailsKey errorData 3600

  return Success

----------- DigiLocker Fetch Documents API -----------

postDriverDocumentsVerifyDigilocker ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    API.Types.UI.DriverOnboardingV2.DigiLockerPullDocRequest ->
    Environment.Flow APISuccess
  )
postDriverDocumentsVerifyDigilocker (mbPersonId, merchantId, merchantOpCityId) req = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")

  logInfo $ "DigiLocker pull - Received request for DocType: " <> req.docType <> ", DriverId: " <> personId.getId

  -- Fetch DigiLocker details from Redis
  let detailsKey = mkDigiLockerDetailsKey personId
  mbDetailsData <- Redis.get detailsKey :: Flow (Maybe APITypes.DigiLockerDetailsData)
  detailsData <- mbDetailsData & fromMaybeM (InvalidRequest "DigiLocker session not found. Please authenticate with DigiLocker first.")

  -- Extract access token
  let mbAccessToken = APITypes.accessToken (APITypes.digilocker detailsData)
  accessToken <- mbAccessToken & fromMaybeM (InvalidRequest "DigiLocker access token not found. Please re-authenticate.")

  logInfo $ "DigiLocker pull - Access token found. DriverId: " <> personId.getId

  -- Return success immediately and process in background
  fork "digilocker-pull-document" $ do
    processDigiLockerPullDocument personId merchantId merchantOpCityId accessToken req detailsKey
      `catchAny` \err -> do
        logError $ "DigiLocker pull document failed - DocType: " <> req.docType <> ", Error: " <> show err <> ", DriverId: " <> personId.getId
        -- Update Redis with error
        updateDocumentStatus detailsKey req.docType "FAILED" "UNKNOWN" Nothing (Just "PROCESSING_FAILED") (Just $ T.pack $ show err)

  return Success

-- Process document pull in background
processDigiLockerPullDocument ::
  Id Domain.Types.Person.Person ->
  Id Domain.Types.Merchant.Merchant ->
  Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity ->
  Text -> -- access token
  APITypes.DigiLockerPullDocRequest ->
  Text -> -- details Redis key
  Flow ()
processDigiLockerPullDocument driverId merchantId merchantOpCityId accessToken req detailsKey = do
  logInfo $ "DigiLocker pull processing - Starting for DocType: " <> req.docType <> ", DriverId: " <> driverId.getId

  -- Update status to PENDING
  updateDocumentStatus detailsKey req.docType "PENDING" "PULL" Nothing Nothing Nothing

  -- Parse document parameters and construct URI based on document type
  let docUri = case req.docParams of
        APITypes.PanParams (APITypes.PanDocParams {APITypes.panNumber = panNum}) ->
          "in.gov.pan-PANCR-" <> panNum
        APITypes.DLParams (APITypes.DLDocParams {APITypes.dlNumber = dlNum}) ->
          "in.gov.transport-DRVLC-" <> dlNum
        APITypes.AadhaarParams (APITypes.AadhaarDocParams {APITypes.aadhaarNumber = aadhaarNum}) ->
          "in.gov.uidai-ADHAR-" <> aadhaarNum

  logInfo $ "DigiLocker pull processing - Constructed URI: " <> docUri <> ", DriverId: " <> driverId.getId

  -- Determine image/document type for validateImage
  let documentType = case req.docParams of
        APITypes.PanParams _ -> DTO.PanCard
        APITypes.DLParams _ -> DTO.DriverLicense
        APITypes.AadhaarParams _ -> DTO.AadhaarCard

  -- Fetch PDF/image from DigiLocker and upload using existing validateImage logic
  let pdfUrl = "https://digilocker.meripehchaan.gov.in/public/oauth2/1/file/" <> docUri
  pdfDataBase64 <- fetchDigiLockerDocument driverId merchantId merchantOpCityId pdfUrl accessToken

  logInfo $ "DigiLocker pull processing - Fetched PDF successfully. DriverId: " <> driverId.getId

  -- Use existing validateImage function to upload to S3 and create Image record
  let imageValidateReq =
        Image.ImageValidateRequest
          { image = pdfDataBase64,
            imageType = documentType,
            rcNumber = Nothing,
            validationStatus = Just APITypes.AUTO_APPROVED, -- DigiLocker docs are pre-validated
            workflowTransactionId = Nothing,
            vehicleCategory = Nothing,
            sdkFailureReason = Nothing
          }

  Image.ImageValidateResponse {imageId} <- Image.validateImage False (driverId, merchantId, merchantOpCityId) imageValidateReq

  logInfo $ "DigiLocker pull processing - Uploaded to S3 via validateImage, ImageId: " <> imageId.getId <> ", DriverId: " <> driverId.getId

  -- Fetch XML document from DigiLocker
  let xmlUrl = "https://digilocker.meripehchaan.gov.in/public/oauth2/1/xml/" <> docUri
  xmlData <- fetchDigiLockerDocument driverId merchantId merchantOpCityId xmlUrl accessToken

  logInfo $ "DigiLocker pull processing - Fetched XML successfully. DriverId: " <> driverId.getId

  -- Parse XML and store in DB based on document type
  case req.docParams of
    APITypes.PanParams params ->
      storePanDocument driverId merchantId merchantOpCityId xmlData imageId params
    APITypes.DLParams params ->
      storeDLDocument driverId merchantId merchantOpCityId xmlData imageId params
    APITypes.AadhaarParams params ->
      storeAadhaarDocument driverId merchantId merchantOpCityId xmlData imageId params

  -- Update Redis status to VALID
  updateDocumentStatus detailsKey req.docType "VALID" "ISSUED" Nothing Nothing Nothing

  logInfo $ "DigiLocker pull processing - Completed successfully for DocType: " <> req.docType <> ", DriverId: " <> driverId.getId

-- Helper: Fetch document from DigiLocker
-- Returns base64-encoded document data
fetchDigiLockerDocument ::
  Id Domain.Types.Person.Person ->
  Id Domain.Types.Merchant.Merchant ->
  Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity ->
  Text ->
  Text ->
  Flow Text
fetchDigiLockerDocument _driverId _merchantId _merchantOpCityId url accessToken = do
  logInfo $ "Fetching document from DigiLocker: " <> url

  -- Parse the URL to extract base URL and path
  baseUrl <- Kernel.Prelude.parseBaseUrl url

  -- Prepare authorization header with Bearer token
  let authHeader = "Bearer " <> accessToken
      eulerClient = Euler.client digiLockerFileAPI (Just authHeader)

  -- Make HTTP call to DigiLocker
  response <-
    try @_ @SomeException (callAPI baseUrl eulerClient "digilocker-fetch-document" digiLockerFileAPI)
      >>= \case
        Left err -> do
          logError $ "DigiLocker fetch failed for URL: " <> url <> ", Error: " <> show err
          -- Log FETCH_DOCUMENT flow (error)
          -- TODO: Add DigiLocker logging
          -- createDigiLockerLog removed - will be replaced with new logging approach
          throwError $ InternalError $ "Failed to fetch document from DigiLocker: " <> T.pack (show err)
        Right eitherResp -> case eitherResp of
          Left clientErr -> do
            logError $ "DigiLocker API error for URL: " <> url <> ", Error: " <> show clientErr
            -- Log FETCH_DOCUMENT flow (API error)
            -- TODO: Add DigiLocker logging
            -- createDigiLockerLog removed - will be replaced with new logging approach
            throwError $ InternalError $ "DigiLocker API returned error: " <> T.pack (show clientErr)
          Right rawData -> do
            logInfo $ "Successfully fetched document from DigiLocker: " <> url
            -- Log FETCH_DOCUMENT flow (success)
            -- TODO: Add DigiLocker logging
            -- createDigiLockerLog removed - will be replaced with new logging approach
            -- If data is already base64, return as-is; otherwise encode it
            -- DigiLocker file endpoint returns base64-encoded PDF data
            return rawData

  return response

-- Helper: Parse PAN date format (DD-MM-YYYY) to UTCTime
parsePanDate :: Text -> Maybe UTCTime
parsePanDate dateStr = do
  parsed <- parseTimeM True defaultTimeLocale "%d-%m-%Y" (T.unpack dateStr) :: Maybe DT.Day
  return $ DT.UTCTime parsed 0

-- Helper: Extract PAN details from DigiLocker XML
data PanXmlData = PanXmlData
  { panNumber :: Text,
    holderName :: Maybe Text,
    dateOfBirth :: Maybe UTCTime
  }

parsePanXml :: Text -> Either Text PanXmlData
parsePanXml xmlText = do
  -- Parse XML document
  doc <- case XML.parseText XML.def (TL.fromStrict xmlText) of
    Left err -> Left $ "Failed to parse XML: " <> T.pack (show err)
    Right d -> Right d

  let cursor = XML.fromDocument doc
      -- Extract PAN number from Certificate/@number attribute
      panNum = listToMaybe $ cursor XML.$| XML.laxElement "Certificate" XML.>=> XML.attribute "number"
      -- Extract name from IssuedTo/Person/@name
      name = listToMaybe $ cursor XML.$// XML.laxElement "IssuedTo" XML.&/ XML.laxElement "Person" XML.>=> XML.attribute "name"
      -- Extract DOB from IssuedTo/Person/@dob
      dobStr = listToMaybe $ cursor XML.$// XML.laxElement "IssuedTo" XML.&/ XML.laxElement "Person" XML.>=> XML.attribute "dob"

  -- Validate and return
  case panNum of
    Nothing -> Left "PAN number not found in XML"
    Just pan ->
      Right $
        PanXmlData
          { panNumber = pan,
            holderName = name,
            dateOfBirth = dobStr >>= parsePanDate
          }

-- Helper: Store PAN document in DB
storePanDocument ::
  Id Domain.Types.Person.Person ->
  Id Domain.Types.Merchant.Merchant ->
  Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity ->
  Text -> -- XML data
  Id Image.Image ->
  APITypes.PanDocParams ->
  Flow ()
storePanDocument driverId merchantId merchantOpCityId xmlData imageId (APITypes.PanDocParams {APITypes.panNumber = panNum}) = do
  -- Parse XML to extract PAN details
  panXmlData <- case parsePanXml xmlData of
    Left err -> do
      logError $ "Failed to parse PAN XML: " <> err
      throwError $ InvalidRequest $ "Failed to parse PAN document: " <> err
    Right parsed -> return parsed

  -- Validate that the PAN number from XML matches the one provided by user
  unless (panXmlData.panNumber == panNum) $ do
    logWarning $ "PAN number mismatch. XML: " <> panXmlData.panNumber <> ", Provided: " <> panNum
    throwError $ InvalidRequest "PAN number in document does not match the provided PAN number"

  person <- PersonQuery.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)

  -- Build PAN card entity with parsed XML data
  now <- getCurrentTime
  uuid <- generateGUID
  encryptedPan <- encrypt panNum

  let panCard =
        DPC.DriverPanCard
          { id = uuid,
            driverId = driverId,
            panCardNumber = encryptedPan,
            documentImageId1 = imageId,
            documentImageId2 = Nothing,
            driverDob = panXmlData.dateOfBirth,
            driverName = Just person.firstName,
            driverNameOnGovtDB = panXmlData.holderName,
            verificationStatus = Documents.VALID,
            verifiedBy = Just DPC.DIGILOCKER,
            consent = True,
            consentTimestamp = now,
            docType = Nothing,
            failedRules = [],
            merchantId = Just merchantId,
            merchantOperatingCityId = Just merchantOpCityId,
            createdAt = now,
            updatedAt = now
          }

  QDPC.upsertPanRecord panCard

  logInfo $ "DigiLocker pull - PAN card stored successfully. DriverId: " <> driverId.getId <> ", PAN: " <> panNum <> ", Name: " <> show panXmlData.holderName

-- Helper: Extract DL details from DigiLocker XML
data DLXmlData = DLXmlData
  { dlNumber :: Text,
    driverName :: Maybe Text,
    dateOfBirth :: Maybe UTCTime,
    licenseExpiry :: UTCTime,
    dateOfIssue :: Maybe UTCTime,
    categories :: [Text], -- List of vehicle category abbreviations (e.g., ["MCWOG", "MCWG", "LMV"])
    address :: Maybe Text
  }

parseDLXml :: Text -> Either Text DLXmlData
parseDLXml xmlText = do
  -- Parse XML document
  doc <- case XML.parseText XML.def (TL.fromStrict xmlText) of
    Left err -> Left $ "Failed to parse XML: " <> T.pack (show err)
    Right d -> Right d

  let cursor = XML.fromDocument doc
      -- Extract DL number from Certificate/@number attribute
      dlNum = listToMaybe $ cursor XML.$| XML.laxElement "Certificate" XML.>=> XML.attribute "number"
      -- Extract issue date from Certificate/@issueDate
      issueDateStr = listToMaybe $ cursor XML.$| XML.laxElement "Certificate" XML.>=> XML.attribute "issueDate"
      -- Extract expiry date from Certificate/@expiryDate
      expiryDateStr = listToMaybe $ cursor XML.$| XML.laxElement "Certificate" XML.>=> XML.attribute "expiryDate"
      -- Extract name from IssuedTo/Person/@dlNatName or @dlConcateName
      natName = listToMaybe $ cursor XML.$// XML.laxElement "IssuedTo" XML.&/ XML.laxElement "Person" XML.>=> XML.attribute "dlNatName"
      concateName = listToMaybe $ cursor XML.$// XML.laxElement "IssuedTo" XML.&/ XML.laxElement "Person" XML.>=> XML.attribute "dlConcateName"
      name_ = listToMaybe $ cursor XML.$// XML.laxElement "IssuedTo" XML.&/ XML.laxElement "Person" XML.>=> XML.attribute "name"
      -- Extract DOB from IssuedTo/Person/@dob
      dobStr = listToMaybe $ cursor XML.$// XML.laxElement "IssuedTo" XML.&/ XML.laxElement "Person" XML.>=> XML.attribute "dob"
      -- Extract address from IssuedTo/Person/Address/@line1
      addressLine1 = listToMaybe $ cursor XML.$// XML.laxElement "IssuedTo" XML.&/ XML.laxElement "Person" XML.&/ XML.laxElement "Address" XML.>=> XML.attribute "line1"
      -- Extract vehicle categories from CertificateData/DrivingLicense/Categories/Category/@abbreviation
      categoryAbbrs =
        cursor XML.$// XML.laxElement "CertificateData"
          XML.&/ XML.laxElement "DrivingLicense"
          XML.&/ XML.laxElement "Categories"
          XML.&/ XML.laxElement "Category"
          XML.>=> XML.attribute "abbreviation"

  -- Parse and validate
  case (dlNum, expiryDateStr) of
    (Nothing, _) -> Left "DL number not found in XML"
    (_, Nothing) -> Left "DL expiry date not found in XML"
    (Just dl, Just expiryStr) -> do
      expiry <- case parseDLDate expiryStr of
        Nothing -> Left $ "Failed to parse expiry date: " <> expiryStr
        Just d -> Right d

      let dob = dobStr >>= parseDLDate
          issueDate = issueDateStr >>= parseDLDate
          driverName = natName <|> concateName <|> name_
          categories = if null categoryAbbrs then [] else categoryAbbrs

      Right $
        DLXmlData
          { dlNumber = T.strip dl, -- Remove trailing spaces from DL number
            driverName = T.strip <$> driverName,
            dateOfBirth = dob,
            licenseExpiry = expiry,
            dateOfIssue = issueDate,
            categories = map T.toUpper categories, -- Normalize to uppercase
            address = T.strip <$> addressLine1
          }

-- Helper: Parse DL date format (DD-MM-YYYY) to UTCTime
parseDLDate :: Text -> Maybe UTCTime
parseDLDate dateStr = do
  parsed <- parseTimeM True defaultTimeLocale "%d-%m-%Y" (T.unpack dateStr) :: Maybe DT.Day
  return $ DT.UTCTime parsed 0

-- Helper: Validate DL status from DigiLocker data
-- Reuses the same validation logic as the existing verifyDL flow
validateDLStatusFromDigiLocker :: DTO.DocumentVerificationConfig -> UTCTime -> [Text] -> UTCTime -> Documents.VerificationStatus
validateDLStatusFromDigiLocker configs expiry categories now =
  case configs.supportedVehicleClasses of
    DTO.DLValidClasses [] -> Documents.INVALID
    DTO.DLValidClasses validCOVs -> do
      let validCOVsCheck = configs.vehicleClassCheckType
      let isCOVValid = foldr' (\x acc -> isValidCOV validCOVs validCOVsCheck x || acc) False categories
      if ((not configs.checkExpiry) || now < expiry) && isCOVValid
        then Documents.VALID
        else Documents.INVALID
    _ -> Documents.INVALID
  where
    -- Reuse the classCheckFunction from SharedLogic.DriverOnboarding
    isValidCOV :: [Text] -> DTO.VehicleClassCheckType -> Text -> Bool
    isValidCOV validCOVs validCOVsCheck cov =
      foldr' (\x acc -> classCheckFunction validCOVsCheck (T.toUpper x) (T.toUpper cov) || acc) False validCOVs

-- Helper: Store DL document in DB
storeDLDocument ::
  Id Domain.Types.Person.Person ->
  Id Domain.Types.Merchant.Merchant ->
  Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity ->
  Text -> -- XML data
  Id Image.Image ->
  APITypes.DLDocParams ->
  Flow ()
storeDLDocument driverId merchantId merchantOpCityId xmlData imageId (APITypes.DLDocParams {APITypes.dlNumber = dlNum, APITypes.dateOfBirth = dobStr}) = do
  -- Parse XML to extract DL details
  dlXmlData <- case parseDLXml xmlData of
    Left err -> do
      logError $ "Failed to parse DL XML: " <> err
      throwError $ InvalidRequest $ "Failed to parse DL document: " <> err
    Right parsed -> return parsed

  -- Validate that the DL number from XML matches the one provided by user (ignoring spaces and case)
  let normalizeDL = T.toUpper . T.filter (/= ' ')
  unless (normalizeDL dlXmlData.dlNumber == normalizeDL dlNum) $ do
    logWarning $ "DL number mismatch. XML: " <> dlXmlData.dlNumber <> ", Provided: " <> dlNum
    throwError $ InvalidRequest "DL number in document does not match the provided DL number"

  -- Validate DOB if provided
  whenJust dobStr $ \providedDob -> do
    whenJust dlXmlData.dateOfBirth $ \xmlDob -> do
      let normalizeDate dt = formatTime defaultTimeLocale "%d-%m-%Y" dt
      unless (T.pack (normalizeDate xmlDob) == providedDob) $ do
        logWarning $ "DOB mismatch. XML: " <> T.pack (normalizeDate xmlDob) <> ", Provided: " <> providedDob
        throwError $ InvalidRequest "Date of birth in document does not match"

  person <- PersonQuery.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)

  -- Get document verification config for validation
  documentVerificationConfig <-
    CQDVC.findByMerchantOpCityIdAndDocumentTypeAndCategory
      merchantOpCityId
      DTO.DriverLicense
      DVC.CAR -- Default to CAR, TODO: determine from categories
      Nothing
      >>= fromMaybeM (DocumentVerificationConfigNotFound merchantOpCityId.getId (show DTO.DriverLicense))

  -- Build DL entity with parsed XML data
  now <- getCurrentTime
  uuid <- generateGUID
  encryptedDL <- encrypt dlNum

  -- Determine verification status based on config and expiry
  let verificationStatus =
        if documentVerificationConfig.doStrictVerifcation
          then validateDLStatusFromDigiLocker documentVerificationConfig dlXmlData.licenseExpiry dlXmlData.categories now
          else Documents.VALID -- DigiLocker verified documents are valid by default
  let driverLicense =
        Domain.Types.DriverLicense.DriverLicense
          { id = uuid,
            driverId = driverId,
            documentImageId1 = imageId,
            documentImageId2 = Nothing,
            licenseNumber = encryptedDL,
            licenseExpiry = dlXmlData.licenseExpiry,
            classOfVehicles = dlXmlData.categories, -- List of category abbreviations
            driverDob = dlXmlData.dateOfBirth,
            driverName = dlXmlData.driverName <|> Just person.firstName,
            verificationStatus = verificationStatus,
            failedRules = [],
            dateOfIssue = dlXmlData.dateOfIssue,
            rejectReason = Nothing,
            vehicleCategory = Nothing, -- Will be determined by system
            consent = True,
            consentTimestamp = now,
            merchantId = Just merchantId,
            createdAt = now,
            updatedAt = now
          }

  QDL.upsert driverLicense

  logInfo $ "DigiLocker pull - DL stored successfully. DriverId: " <> driverId.getId <> ", DL: " <> dlNum <> ", Name: " <> show dlXmlData.driverName

-- Helper: Store Aadhaar document in DB
storeAadhaarDocument ::
  Id Domain.Types.Person.Person ->
  Id Domain.Types.Merchant.Merchant ->
  Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity ->
  Text -> -- XML data
  Id Image.Image ->
  APITypes.AadhaarDocParams ->
  Flow ()
storeAadhaarDocument _driverId _merchantId _merchantOpCityId _xmlData _imageId _params = do
  -- TODO: Parse XML to extract Aadhaar details
  -- For now, throwing error as implementation is pending
  throwError $ InternalError "Aadhaar document storage from DigiLocker not yet implemented"

-- Helper: Update document status in Redis
updateDocumentStatus ::
  Text -> -- details key
  Text -> -- doc type
  Text -> -- status
  Text -> -- availability
  Maybe [Text] -> -- pull fields
  Maybe Text -> -- error code
  Maybe Text -> -- error description
  Flow ()
updateDocumentStatus detailsKey docType status availability pullFields errorCode errorDesc = do
  mbDetailsData <- Redis.get detailsKey :: Flow (Maybe APITypes.DigiLockerDetailsData)
  whenJust mbDetailsData $ \detailsData -> do
    let currentDocs = APITypes.documents detailsData
    let updatedDocs = map updateDoc currentDocs
    let updatedData = detailsData {APITypes.documents = updatedDocs}
    Redis.setExp detailsKey updatedData 3600
  where
    updateDoc doc@(APITypes.DigiLockerDocumentStatus _ docTypeField _ _ _ _) =
      if docTypeField == docType
        then
          doc{APITypes.status = status,
              APITypes.availability = availability,
              APITypes.pullFields = pullFields,
              APITypes.errorCode = errorCode,
              APITypes.errorDescription = errorDesc
             }
        else doc

----------- GET DIGILOCKER AUTHORIZATION URL -----------

postDriverDigilockerInitiate ::
  ( Maybe (Id Domain.Types.Person.Person),
    Id Domain.Types.Merchant.Merchant,
    Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
  ) ->
  Environment.Flow APITypes.DigiLockerInitiateResp
postDriverDigilockerInitiate (mbDriverId, merchantId, merchantOpCityId) = do
  driverId <- mbDriverId & fromMaybeM (PersonNotFound "No person found")
  logInfo $ "DigiLocker initiate - Starting authorization flow for DriverId: " <> driverId.getId

  -- Step 1: Verify DigiLocker is enabled for this merchant+city
  verifyDigiLockerEnabled merchantOpCityId

  -- Step 2: Check for existing active session
  latestSession <- QDV.findLatestByDriverId (Just 1) (Just 0) driverId

  case latestSession of
    [] -> do
      -- No existing session - create new one
      logInfo $ "DigiLocker initiate - No existing session found for DriverId: " <> driverId.getId
      createNewDigiLockerSession driverId merchantId merchantOpCityId
    (session : _) -> do
      -- Existing session found - validate and decide action
      logInfo $ "DigiLocker initiate - Found existing session for DriverId: " <> driverId.getId <> ", SessionStatus: " <> show session.sessionStatus
      handleExistingSession driverId merchantId merchantOpCityId session

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
  Flow APITypes.DigiLockerInitiateResp
handleExistingSession driverId merchantId merchantOpCityId session = do
  now <- getCurrentTime

  case session.sessionStatus of
    DDV.PENDING -> handlePendingSession driverId merchantId merchantOpCityId session now
    DDV.SUCCESS -> handleSuccessSession driverId merchantId merchantOpCityId session now
    DDV.FAILED -> do
      logInfo $ "DigiLocker initiate - Previous session FAILED, creating new session for DriverId: " <> driverId.getId
      createNewDigiLockerSession driverId merchantId merchantOpCityId
    DDV.CONSENT_DENIED -> do
      logInfo $ "DigiLocker initiate - Previous session CONSENT_DENIED, creating new session for DriverId: " <> driverId.getId
      createNewDigiLockerSession driverId merchantId merchantOpCityId

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
  Flow APITypes.DigiLockerInitiateResp
handleSuccessSession driverId merchantId merchantOpCityId session now = do
  -- Check document statuses in docStatus JSON
  checkDocumentStatuses driverId merchantId merchantOpCityId session now

-- Check document statuses and decide action
checkDocumentStatuses ::
  Id Domain.Types.Person.Person ->
  Id Domain.Types.Merchant.Merchant ->
  Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity ->
  DDV.DigilockerVerification ->
  UTCTime ->
  Flow APITypes.DigiLockerInitiateResp
checkDocumentStatuses driverId merchantId merchantOpCityId session now = do
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
      createNewDigiLockerSession driverId merchantId merchantOpCityId
    else -- Check for PULL_REQUIRED documents

      if hasDocWithStatus docStatusMap "PULL_REQUIRED"
        then handlePullRequiredDocs driverId merchantId merchantOpCityId session docStatusMap now
        else -- If no problematic status, check actual document tables
          checkActualDocumentTables driverId merchantId merchantOpCityId

-- Handle PULL_REQUIRED documents
handlePullRequiredDocs ::
  Id Domain.Types.Person.Person ->
  Id Domain.Types.Merchant.Merchant ->
  Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity ->
  DDV.DigilockerVerification ->
  Value ->
  UTCTime ->
  Flow APITypes.DigiLockerInitiateResp
handlePullRequiredDocs driverId merchantId merchantOpCityId session _docStatusMap now = do
  case session.accessTokenExpiresAt of
    Nothing -> do
      logInfo $ "DigiLocker initiate - PULL_REQUIRED with no expiry, creating new session"
      createNewDigiLockerSession driverId merchantId merchantOpCityId
    Just expiresAt ->
      if now < expiresAt
        then do
          logInfo $ "DigiLocker initiate - PULL_REQUIRED with valid token, returning 409"
          throwError DigiLockerPullRequired
        else do
          logInfo $ "DigiLocker initiate - PULL_REQUIRED with expired token, creating new session"
          createNewDigiLockerSession driverId merchantId merchantOpCityId

-- Check actual document tables for verification status
checkActualDocumentTables ::
  Id Domain.Types.Person.Person ->
  Id Domain.Types.Merchant.Merchant ->
  Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity ->
  Flow APITypes.DigiLockerInitiateResp
checkActualDocumentTables driverId merchantId merchantOpCityId = do
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
      createNewDigiLockerSession driverId merchantId merchantOpCityId

-- Create new DigiLocker session
createNewDigiLockerSession ::
  Id Domain.Types.Person.Person ->
  Id Domain.Types.Merchant.Merchant ->
  Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity ->
  Flow APITypes.DigiLockerInitiateResp
createNewDigiLockerSession driverId merchantId merchantOpCityId = do
  logInfo $ "DigiLocker initiate - Creating new session for DriverId: " <> driverId.getId

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

  -- Create session record in DB
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
            merchantId = Just merchantId,
            merchantOperatingCityId = Just merchantOpCityId,
            createdAt = now,
            updatedAt = now
          }

  QDV.create newSession

  logInfo $ "DigiLocker initiate - Created session with ID: " <> sessionId.getId <> ", StateId: " <> stateId

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

----------- DigiLocker Pull Driving License API -----------

postDriverPull_documentDriving_license ::
  ( Maybe (Id Domain.Types.Person.Person),
    Id Domain.Types.Merchant.Merchant,
    Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
  ) ->
  APITypes.PullDrivingLicenseReq ->
  Environment.Flow APISuccess
postDriverPull_documentDriving_license (mbDriverId, merchantId, merchantOpCityId) req = do
  driverId <- mbDriverId & fromMaybeM (PersonNotFound "No person found")
  logInfo $ "DigiLocker pull DL - Starting for DriverId: " <> driverId.getId <> ", DL: " <> req.dlno

  -- Fetch DigiLocker access token from Redis
  let detailsKey = mkDigiLockerDetailsKey driverId
  mbDetailsData <- Redis.get detailsKey :: Flow (Maybe APITypes.DigiLockerDetailsData)
  detailsData <- mbDetailsData & fromMaybeM (InvalidRequest "DigiLocker session not found. Please authenticate with DigiLocker first.")

  -- Extract access token
  let mbAccessToken = APITypes.accessToken (APITypes.digilocker detailsData)
  accessToken <- mbAccessToken & fromMaybeM (InvalidRequest "DigiLocker access token not found. Please re-authenticate.")

  logInfo $ "DigiLocker pull DL - Access token found. DriverId: " <> driverId.getId

  -- Return success immediately and process in background
  fork "digilocker-pull-driving-license" $ do
    processPullDrivingLicense driverId merchantId merchantOpCityId accessToken req detailsKey
      `catchAny` \err -> do
        logError $ "DigiLocker pull DL failed - Error: " <> show err <> ", DriverId: " <> driverId.getId
        -- Update DB with error status
        handleDrivingLicenseError driverId merchantId merchantOpCityId (T.pack $ show err) "PROCESSING_FAILED"

  return Success

-- Process pull driving license in background
processPullDrivingLicense ::
  Id Domain.Types.Person.Person ->
  Id Domain.Types.Merchant.Merchant ->
  Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity ->
  Text -> -- access token
  APITypes.PullDrivingLicenseReq ->
  Text -> -- details Redis key
  Flow ()
processPullDrivingLicense driverId merchantId merchantOpCityId accessToken req detailsKey = do
  logInfo $ "DigiLocker pull DL processing - Starting for DriverId: " <> driverId.getId <> ", DL: " <> req.dlno

  -- Update DL status to PENDING in DB
  updateDLStatusInDB driverId "PENDING" []

  -- Step 1: Call DigiLocker pull document API to get URI
  pullResult <- callDigiLockerPullDocumentAPI driverId merchantId merchantOpCityId accessToken "DRVLC" req.dlno

  case pullResult of
    Left errorInfo -> do
      -- Handle DigiLocker API errors
      logError $ "DigiLocker pull DL - API error: " <> errorInfo.errorCode <> " - " <> errorInfo.errorMsg
      handleDrivingLicenseError driverId merchantId merchantOpCityId errorInfo.errorMsg errorInfo.errorCode
    Right _docUri -> do
      logInfo $ "DigiLocker pull DL - URI received, using existing verification flow. DriverId: " <> driverId.getId

      -- Step 2: Use existing processDigiLockerPullDocument function
      let pullDocRequest =
            APITypes.DigiLockerPullDocRequest
              { docType = "DRVLC",
                docParams = APITypes.DLParams $ APITypes.DLDocParams {APITypes.dlNumber = req.dlno, APITypes.dateOfBirth = Nothing}
              }

      processDigiLockerPullDocument driverId merchantId merchantOpCityId accessToken pullDocRequest detailsKey
        `catchAny` \err -> do
          logError $ "DigiLocker pull DL - Verification failed: " <> show err <> ", DriverId: " <> driverId.getId
          handleDrivingLicenseError driverId merchantId merchantOpCityId (T.pack $ show err) "unexpected_error"

-- Call DigiLocker pull document API
data PullDocErrorInfo = PullDocErrorInfo
  { errorCode :: Text,
    errorMsg :: Text
  }

callDigiLockerPullDocumentAPI ::
  Id Domain.Types.Person.Person ->
  Id Domain.Types.Merchant.Merchant ->
  Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity ->
  Text -> -- access token
  Text -> -- doctype (e.g., "DRVLC")
  Text -> -- document number (e.g., DL number)
  Flow (Either PullDocErrorInfo Text) -- Either error or URI
callDigiLockerPullDocumentAPI _driverId _merchantId _merchantOpCityId accessToken doctype docNumber = do
  let pullUrl = "https://digilocker.meripehchaan.gov.in/public/oauth2/1/pull/pulldocument"

  logInfo $ "DigiLocker pull API - Calling for doctype: " <> doctype <> ", docNumber: " <> docNumber

  -- Prepare request body (form-urlencoded)
  let formData =
        [ ("orgid", "000048"),
          ("doctype", doctype),
          ("consent", "Y"),
          ("dlno", docNumber)
        ]

  -- Prepare authorization header
  let authHeader = "Bearer " <> accessToken
      contentTypeHeader = "application/x-www-form-urlencoded"

  -- Parse URL
  baseUrl <- Kernel.Prelude.parseBaseUrl pullUrl

  -- Make HTTP call
  let eulerClient = Euler.client digiLockerPullDocAPI (Just authHeader) (Just contentTypeHeader) formData

  result <- try @_ @SomeException (callAPI baseUrl eulerClient "digilocker-pull-document" digiLockerPullDocAPI)

  case result of
    Left err -> do
      logError $ "DigiLocker pull API - HTTP error: " <> show err
      -- Log PULL_DOCUMENT flow (HTTP error)
      -- TODO: Add DigiLocker logging
      -- createDigiLockerLog removed - will be replaced with new logging approach
      return $ Left $ PullDocErrorInfo "unexpected_error" (T.pack $ show err)
    Right eitherResp -> case eitherResp of
      Left clientErr -> do
        logError $ "DigiLocker pull API - Client error: " <> show clientErr
        -- Try to extract error code from response
        let errMsg = T.pack $ show clientErr
        let errCode = extractErrorCodeFromResponse errMsg
        -- Log PULL_DOCUMENT flow (client error)
        -- TODO: Add DigiLocker logging
        -- createDigiLockerLog removed - will be replaced with new logging approach
        return $ Left $ PullDocErrorInfo errCode errMsg
      Right pullResp -> do
        case pullResp.uri of
          Just docUri -> do
            logInfo $ "DigiLocker pull API - Success, URI: " <> docUri
            -- Log PULL_DOCUMENT flow (success)
            -- TODO: Add DigiLocker logging
            -- createDigiLockerLog removed - will be replaced with new logging approach
            return $ Right docUri
          Nothing -> do
            let errCode = fromMaybe "uri_missing" pullResp._error
            let errMsg = fromMaybe "URI not found in response" pullResp.error_description
            logError $ "DigiLocker pull API - Error: " <> errCode <> " - " <> errMsg
            -- Log PULL_DOCUMENT flow (error - no URI)
            -- TODO: Add DigiLocker logging
            -- createDigiLockerLog removed - will be replaced with new logging approach
            return $ Left $ PullDocErrorInfo errCode errMsg

-- Extract error code from DigiLocker API error response
extractErrorCodeFromResponse :: Text -> Text
extractErrorCodeFromResponse responseText
  | T.isInfixOf "401" responseText || T.isInfixOf "invalid_token" responseText = "invalid_token"
  | T.isInfixOf "403" responseText || T.isInfixOf "insufficient_scope" responseText = "insufficient_scope"
  | T.isInfixOf "400" responseText && T.isInfixOf "invalid_orgid" responseText = "invalid_orgid"
  | T.isInfixOf "400" responseText && T.isInfixOf "invalid_doctype" responseText = "invalid_doctype"
  | T.isInfixOf "500" responseText && T.isInfixOf "repository_service" responseText = "repository_service_configerror"
  | T.isInfixOf "400" responseText && T.isInfixOf "pull_response_pending" responseText = "pull_response_pending"
  | T.isInfixOf "400" responseText && T.isInfixOf "uri_exists" responseText = "uri_exists"
  | T.isInfixOf "404" responseText && T.isInfixOf "record_not_found" responseText = "record_not_found"
  | T.isInfixOf "400" responseText && T.isInfixOf "aadhaar_not_linked" responseText = "aadhaar_not_linked"
  | T.isInfixOf "500" responseText = "unexpected_error"
  | otherwise = "unknown_error"

-- Update DL status in DB
updateDLStatusInDB :: Id Domain.Types.Person.Person -> Text -> [Text] -> Flow ()
updateDLStatusInDB driverId status failedRules = do
  mbDL <- QDL.findByDriverId driverId
  whenJust mbDL $ \dl -> do
    now <- getCurrentTime
    let verificationStatus = case status of
          "PENDING" -> Documents.PENDING
          "VALID" -> Documents.VALID
          "INVALID" -> Documents.INVALID
          "MANUAL_VERIFICATION_REQUIRED" -> Documents.MANUAL_VERIFICATION_REQUIRED
          "UNAUTHORIZED" -> Documents.UNAUTHORIZED
          "PULL_REQUIRED" -> Documents.PULL_REQUIRED
          _ -> Documents.INVALID

    let updatedDL =
          dl
            { DDL.verificationStatus = verificationStatus,
              DDL.failedRules = failedRules,
              DDL.updatedAt = now
            }
    QDL.upsert updatedDL

-- Handle driving license error
handleDrivingLicenseError ::
  Id Domain.Types.Person.Person ->
  Id Domain.Types.Merchant.Merchant ->
  Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity ->
  Text -> -- error message
  Text -> -- error code from DigiLocker
  Flow ()
handleDrivingLicenseError driverId _merchantId _merchantOpCityId errorMsg errorCode = do
  logError $ "Handling DL error - Code: " <> errorCode <> ", Message: " <> errorMsg <> ", DriverId: " <> driverId.getId

  -- Map DigiLocker error codes to verification status and failedRules
  -- Add digilocker_ prefix when storing in DB for consistency
  let (verificationStatus, failedRulesList) = case errorCode of
        "invalid_token" -> ("UNAUTHORIZED", ["digilocker_invalid_token"])
        "insufficient_scope" -> ("UNAUTHORIZED", ["digilocker_insufficient_scope"])
        "invalid_orgid" -> ("INVALID", ["digilocker_invalid_orgid"])
        "invalid_doctype" -> ("INVALID", ["digilocker_invalid_doctype"])
        "repository_service_configerror" -> ("INVALID", ["digilocker_repository_service_configerror"])
        "pull_response_pending" -> ("MANUAL_VERIFICATION_REQUIRED", ["digilocker_pull_response_pending"])
        "uri_exists" -> ("INVALID", ["digilocker_uri_exists"])
        "record_not_found" -> ("INVALID", ["digilocker_record_not_found"])
        "aadhaar_not_linked" -> ("INVALID", ["digilocker_aadhaar_not_linked"])
        "unexpected_error" -> ("INVALID", ["digilocker_unexpected_error"])
        "uri_missing" -> ("INVALID", ["digilocker_uri_missing"])
        _ -> ("INVALID", ["digilocker_" <> errorCode])

  -- Update DB with error
  updateDLStatusInDB driverId verificationStatus failedRulesList
