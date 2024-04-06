{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.DriverOnboarding where

import Control.Applicative ((<|>))
import qualified Data.Text as T
import Data.Time hiding (getCurrentTime)
import qualified Domain.Types.DriverInformation as DI
import Domain.Types.DriverRCAssociation
import Domain.Types.IdfyVerification
import qualified Domain.Types.Image as Domain
import qualified Domain.Types.Merchant as DTM
import qualified Domain.Types.Merchant.MerchantMessage as DMM
import qualified Domain.Types.Merchant.MerchantOperatingCity as DMOC
import Domain.Types.Person
import Domain.Types.Vehicle
import Domain.Types.VehicleRegistrationCertificate
import qualified Domain.Types.VehicleServiceTier as DVST
import Environment
import Kernel.External.Encryption (decrypt)
import Kernel.External.Ticket.Interface.Types as Ticket
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.MessageBuilder (addBroadcastMessageToKafka)
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantMessage as QMM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Merchant.TransporterConfig as CQTC
import qualified Storage.Queries.DriverInformation as DIQuery
import qualified Storage.Queries.Image as Query
import qualified Storage.Queries.Message.Message as MessageQuery
import qualified Storage.Queries.Person as QP
import Tools.Error
import qualified Tools.Ticket as TT
import Tools.Whatsapp as Whatsapp

notifyErrorToSupport ::
  Person ->
  Id DTM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Maybe T.Text ->
  T.Text ->
  [Maybe DriverOnboardingError] ->
  Flow ()
notifyErrorToSupport person merchantId merchantOpCityId driverPhone _ errs = do
  transporterConfig <- CQTC.findByMerchantOpCityId merchantOpCityId (Just person.id.getId) (Just "driverId") >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  let reasons = catMaybes $ mapMaybe toMsg errs
  let description = T.intercalate ", " reasons
  _ <- TT.createTicket merchantId merchantOpCityId (mkTicket description transporterConfig.kaptureDisposition)
  return ()
  where
    toMsg e = toMessage <$> e

    mkTicket description disposition =
      Ticket.CreateTicketReq
        { category = "GENERAL",
          subCategory = Just "DRIVER ONBOARDING ISSUE",
          disposition = disposition,
          issueId = Nothing,
          issueDescription = description,
          mediaFiles = Nothing,
          name = Just $ person.firstName <> " " <> fromMaybe "" person.lastName,
          phoneNo = driverPhone,
          personId = person.id.getId,
          classification = Ticket.DRIVER,
          rideDescription = Nothing
        }

throwImageError :: Id Domain.Image -> DriverOnboardingError -> Flow b
throwImageError id_ err = do
  _ <- Query.addFailureReason (Just err) id_
  throwError err

getFreeTrialDaysLeft :: MonadFlow m => Int -> DI.DriverInformation -> m Int
getFreeTrialDaysLeft freeTrialDays driverInfo = do
  now <- getCurrentTime
  let driverEnablementDay = utctDay (fromMaybe now (driverInfo.enabledAt <|> driverInfo.lastEnabledOn))
  return $ max 0 (freeTrialDays - fromInteger (diffDays (utctDay now) driverEnablementDay))

triggerOnboardingAlertsAndMessages :: Person -> DTM.Merchant -> DMOC.MerchantOperatingCity -> Flow ()
triggerOnboardingAlertsAndMessages driver merchant merchantOperatingCity = do
  fork "Triggering onboarding messages" $ do
    -- broadcast messages
    messages <- MessageQuery.findAllOnboardingMessages merchant merchantOperatingCity
    mapM_ (\msg -> addBroadcastMessageToKafka False msg driver.id) messages

    -- whatsapp message
    mobileNumber <- mapM decrypt driver.mobileNumber >>= fromMaybeM (PersonFieldNotPresent "mobileNumber")
    countryCode <- driver.mobileCountryCode & fromMaybeM (PersonFieldNotPresent "mobileCountryCode")
    let phoneNumber = countryCode <> mobileNumber
    merchantMessage <-
      QMM.findByMerchantOpCityIdAndMessageKey merchantOperatingCity.id DMM.WELCOME_TO_PLATFORM
        >>= fromMaybeM (MerchantMessageNotFound merchantOperatingCity.id.getId (show DMM.WELCOME_TO_PLATFORM))
    let jsonData = merchantMessage.jsonData
    result <- Whatsapp.whatsAppSendMessageWithTemplateIdAPI driver.merchantId merchantOperatingCity.id (Whatsapp.SendWhatsAppMessageWithTemplateIdApIReq phoneNumber merchantMessage.templateId jsonData.var1 jsonData.var2 jsonData.var3 Nothing (Just merchantMessage.containsUrlButton))
    when (result._response.status /= "success") $ throwError (InternalError "Unable to send Whatsapp message via dashboard")

enableAndTriggerOnboardingAlertsAndMessages :: Id DMOC.MerchantOperatingCity -> Id Person -> Bool -> Flow ()
enableAndTriggerOnboardingAlertsAndMessages merchantOpCityId personId verified = do
  driverInfo <- DIQuery.findById (cast personId) >>= fromMaybeM (PersonNotFound personId.getId)
  DIQuery.updateEnabledVerifiedState personId True (Just verified)
  when (not driverInfo.enabled && isNothing driverInfo.enabledAt) $ do
    merchantOpCity <- CQMOC.findById merchantOpCityId >>= fromMaybeM (MerchantOperatingCityNotFound merchantOpCityId.getId)
    merchant <- CQM.findById merchantOpCity.merchantId >>= fromMaybeM (MerchantNotFound merchantOpCity.merchantId.getId)
    person <- QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
    triggerOnboardingAlertsAndMessages person merchant merchantOpCity

selectVehicleTierForDriver :: Person -> DI.DriverInformation -> Vehicle -> [DVST.VehicleServiceTier] -> [DVST.VehicleServiceTier]
selectVehicleTierForDriver person driverInfo vehicle cityVehicleServiceTiers =
  filter filterVehicleTier cityVehicleServiceTiers
  where
    filterVehicleTier vehicleServiceTier = do
      let seatingCapacityCheck = compareNumber vehicle.capacity vehicleServiceTier.seatingCapacity
      let luggageCapacityCheck = compareNumber vehicle.luggageCapacity vehicleServiceTier.luggageCapacity
      let airConditionedCheck =
            (compareNumber driverInfo.airConditionScore vehicleServiceTier.airConditioned)
              && (isNothing vehicleServiceTier.airConditioned || vehicle.airConditioned /= Just False)
      let driverRatingCheck = compareNumber person.rating vehicleServiceTier.driverRating
      let vehicleRatingCheck = compareNumber vehicle.vehicleRating vehicleServiceTier.vehicleRating
      let variantCheck = vehicle.variant `elem` vehicleServiceTier.allowedVehicleVariant

      seatingCapacityCheck && luggageCapacityCheck && airConditionedCheck && driverRatingCheck && vehicleRatingCheck && variantCheck

    compareNumber :: Ord a => Maybe a -> Maybe a -> Bool
    compareNumber mbX mbY =
      case (mbX, mbY) of
        (Just x, Just y) -> x >= y
        _ -> True

makeRCAssociation :: (MonadFlow m) => Id DTM.Merchant -> Id DMOC.MerchantOperatingCity -> Id Person -> Id VehicleRegistrationCertificate -> Maybe UTCTime -> m DriverRCAssociation
makeRCAssociation merchantId merchantOperatingCityId driverId rcId end = do
  id <- generateGUID
  now <- getCurrentTime
  return $
    DriverRCAssociation
      { id,
        driverId,
        rcId,
        associatedOn = now,
        associatedTill = end,
        consent = True,
        consentTimestamp = now,
        isRcActive = False,
        merchantId = Just merchantId,
        merchantOperatingCityId = Just merchantOperatingCityId,
        createdAt = now,
        updatedAt = now
      }

data VehicleRegistrationCertificateAPIEntity = VehicleRegistrationCertificateAPIEntity
  { certificateNumber :: Text,
    fitnessExpiry :: UTCTime,
    permitExpiry :: Maybe UTCTime,
    pucExpiry :: Maybe UTCTime,
    insuranceValidity :: Maybe UTCTime,
    vehicleClass :: Maybe Text,
    vehicleVariant :: Maybe Variant,
    failedRules :: [Text],
    vehicleManufacturer :: Maybe Text,
    vehicleCapacity :: Maybe Int,
    vehicleModel :: Maybe Text,
    manufacturerModel :: Maybe Text,
    reviewRequired :: Maybe Bool,
    vehicleColor :: Maybe Text,
    vehicleEnergyType :: Maybe Text,
    reviewedAt :: Maybe UTCTime,
    verificationStatus :: VerificationStatus,
    fleetOwnerId :: Maybe Text,
    createdAt :: UTCTime
  }
  deriving (Generic, ToSchema, ToJSON, FromJSON)

makeRCAPIEntity :: VehicleRegistrationCertificate -> Text -> VehicleRegistrationCertificateAPIEntity
makeRCAPIEntity VehicleRegistrationCertificate {..} rcDecrypted =
  VehicleRegistrationCertificateAPIEntity
    { certificateNumber = rcDecrypted,
      ..
    }

makeVehicleFromRC :: UTCTime -> Id Person -> Id DTM.Merchant -> Text -> VehicleRegistrationCertificate -> Id DMOC.MerchantOperatingCity -> Vehicle
makeVehicleFromRC now driverId merchantId certificateNumber rc merchantOpCityId =
  Vehicle
    { driverId,
      capacity = rc.vehicleCapacity,
      category = getCategory <$> rc.vehicleVariant,
      make = rc.vehicleManufacturer,
      model = fromMaybe "Unkown" rc.vehicleModel,
      size = Nothing,
      merchantId,
      variant = fromMaybe AUTO_RICKSHAW rc.vehicleVariant, -- Value will be always Just if reaching here
      color = fromMaybe "Unkown" rc.vehicleColor,
      energyType = rc.vehicleEnergyType,
      registrationNo = certificateNumber,
      registrationCategory = Nothing,
      vehicleClass = fromMaybe "Unkown" rc.vehicleClass,
      merchantOperatingCityId = Just merchantOpCityId,
      vehicleName = Nothing,
      airConditioned = rc.airConditioned,
      luggageCapacity = rc.luggageCapacity,
      vehicleRating = rc.vehicleRating,
      createdAt = now,
      updatedAt = now
    }

makeVehicleAPIEntity :: Vehicle -> VehicleAPIEntity
makeVehicleAPIEntity Vehicle {..} = VehicleAPIEntity {..}

getCategory :: Variant -> Category
getCategory SEDAN = CAR
getCategory SUV = CAR
getCategory HATCHBACK = CAR
getCategory AUTO_RICKSHAW = AUTO_CATEGORY
getCategory TAXI = CAR
getCategory TAXI_PLUS = CAR
