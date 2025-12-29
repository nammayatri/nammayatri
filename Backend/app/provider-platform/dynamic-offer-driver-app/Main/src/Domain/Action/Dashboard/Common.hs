module Domain.Action.Dashboard.Common
  ( mapServiceName,
    castVerificationStatus,
    castVehicleVariantDashboard,
    notifyYatriRentalEventsToDriver,
    runVerifyRCFlow,
    appendPlusInMobileCountryCode,
    castStatus,
    checkFleetOwnerVerification,
    checkFleetOwnerRole,
    castDashboardVehicleVariantToDomain,
  )
where

import qualified "dashboard-helper-api" API.Types.ProviderPlatform.Fleet.Driver as Common
import qualified Data.Text as T
import Data.Time hiding (getCurrentTime, secondsToNominalDiffTime)
import qualified Domain.Action.UI.DriverOnboarding.VehicleRegistrationCertificate as DomainRC
import Domain.Types.DriverFee as DDF
import qualified Domain.Types.Merchant as DM
import Domain.Types.MerchantMessage (MediaChannel (..), MessageKey (..))
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import Domain.Types.Plan
import Domain.Types.TransporterConfig
import qualified Domain.Types.VehicleVariant as DV
import Environment
import Kernel.Beam.Functions as B
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context as Context
import qualified Kernel.Types.Documents as Documents
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.MessageBuilder as MessageBuilder
import qualified Storage.CachedQueries.Merchant.MerchantMessage as QMM
import qualified Storage.Queries.FleetOwnerInformation as QFI
import qualified Storage.Queries.Person as QPerson
import Tools.Error
import qualified Tools.SMS as Sms
import Tools.Whatsapp as Whatsapp

mapServiceName :: Common.ServiceNames -> ServiceNames
mapServiceName common = case common of
  Common.YATRI_SUBSCRIPTION -> YATRI_SUBSCRIPTION
  Common.YATRI_RENTAL -> YATRI_RENTAL
  Common.PREPAID_SUBSCRIPTION -> PREPAID_SUBSCRIPTION
  Common.DASHCAM_RENTAL_CAUTIO -> DASHCAM_RENTAL CAUTIO

castVerificationStatus :: Documents.VerificationStatus -> Common.VerificationStatus
castVerificationStatus = \case
  Documents.PENDING -> Common.PENDING
  Documents.VALID -> Common.VALID
  Documents.MANUAL_VERIFICATION_REQUIRED -> Common.MANUAL_VERIFICATION_REQUIRED
  Documents.INVALID -> Common.INVALID
  Documents.UNAUTHORIZED -> Common.UNAUTHORIZED

castVehicleVariantDashboard :: Maybe DV.VehicleVariant -> Maybe Common.VehicleVariant
castVehicleVariantDashboard = \case
  Just DV.SUV -> Just Common.SUV
  Just DV.HATCHBACK -> Just Common.HATCHBACK
  Just DV.SEDAN -> Just Common.SEDAN
  Just DV.AUTO_RICKSHAW -> Just Common.AUTO_RICKSHAW
  Just DV.AUTO_PLUS -> Just Common.AUTO_PLUS
  Just DV.TAXI -> Just Common.TAXI
  Just DV.TAXI_PLUS -> Just Common.TAXI_PLUS
  Just DV.PREMIUM_SEDAN -> Just Common.PREMIUM_SEDAN
  Just DV.BLACK -> Just Common.BLACK
  Just DV.BLACK_XL -> Just Common.BLACK_XL
  Just DV.BIKE -> Just Common.BIKE
  Just DV.AMBULANCE_TAXI -> Just Common.AMBULANCE_TAXI
  Just DV.AMBULANCE_TAXI_OXY -> Just Common.AMBULANCE_TAXI_OXY
  Just DV.AMBULANCE_AC -> Just Common.AMBULANCE_AC
  Just DV.AMBULANCE_AC_OXY -> Just Common.AMBULANCE_AC_OXY
  Just DV.AMBULANCE_VENTILATOR -> Just Common.AMBULANCE_VENTILATOR
  Just DV.SUV_PLUS -> Just Common.SUV_PLUS
  Just DV.DELIVERY_BIKE -> Just Common.DELIVERY_BIKE
  Just DV.DELIVERY_LIGHT_GOODS_VEHICLE -> Just Common.DELIVERY_LIGHT_GOODS_VEHICLE
  Just DV.DELIVERY_TRUCK_MINI -> Just Common.DELIVERY_TRUCK_MINI
  Just DV.DELIVERY_TRUCK_SMALL -> Just Common.DELIVERY_TRUCK_SMALL
  Just DV.DELIVERY_TRUCK_MEDIUM -> Just Common.DELIVERY_TRUCK_MEDIUM
  Just DV.DELIVERY_TRUCK_LARGE -> Just Common.DELIVERY_TRUCK_LARGE
  Just DV.DELIVERY_TRUCK_ULTRA_LARGE -> Just Common.DELIVERY_TRUCK_ULTRA_LARGE
  Just DV.BUS_NON_AC -> Just Common.BUS_NON_AC
  Just DV.BUS_AC -> Just Common.BUS_AC
  Just DV.BOAT -> Just Common.BOAT
  Just DV.VIP_ESCORT -> Just Common.VIP_ESCORT
  Just DV.VIP_OFFICER -> Just Common.VIP_OFFICER
  Just DV.BIKE_PLUS -> Just Common.BIKE_PLUS
  Just DV.E_RICKSHAW -> Just Common.E_RICKSHAW
  _ -> Nothing

runVerifyRCFlow :: Id DP.Person -> DM.Merchant -> Id DMOC.MerchantOperatingCity -> Context.City -> Common.AddVehicleReq -> Bool -> Bool -> Maybe (Id DP.Person) -> Flow ()
runVerifyRCFlow personId merchant merchantOpCityId operatingCity req isFleet bulkUpload mbFleetOwnerId = do
  let imageId = maybe "" cast req.imageId
  let rcReq =
        DomainRC.DriverRCReq
          { vehicleRegistrationCertNumber = req.registrationNo,
            imageId = imageId,
            operatingCity = show operatingCity, -- Fixed
            dateOfRegistration = Nothing,
            airConditioned = req.airConditioned,
            oxygen = req.oxygen,
            ventilator = req.ventilator,
            vehicleDetails = Nothing,
            vehicleCategory = req.vehicleCategory,
            isRCImageValidated = Nothing
          }
  void $ DomainRC.verifyRC (not isFleet) (Just merchant) (personId, merchant.id, merchantOpCityId) rcReq bulkUpload mbFleetOwnerId

notifyYatriRentalEventsToDriver :: Maybe Text -> MessageKey -> Id DP.Person -> TransporterConfig -> Maybe Text -> MediaChannel -> Flow ()
notifyYatriRentalEventsToDriver vehicleId messageKey personId transporterConfig mbReason channel = do
  smsCfg <- asks (.smsCfg)
  driver <- B.runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  mobileNumber <- mapM decrypt driver.mobileNumber >>= fromMaybeM (PersonFieldNotPresent "mobileNumber")
  countryCode <- driver.mobileCountryCode & fromMaybeM (PersonFieldNotPresent "mobileCountryCode")
  nowLocale <- getLocalCurrentTime transporterConfig.timeDiffFromUtc
  let phoneNumber = countryCode <> mobileNumber
      timeStamp = show $ utctDay nowLocale
      merchantOpCityId = transporterConfig.merchantOperatingCityId
      mkey = messageKey
  withLogTag ("personId_" <> personId.getId) $ do
    case channel of
      SMS -> do
        (mbSender, message, templateId) <- MessageBuilder.buildGenericMessage merchantOpCityId mkey Nothing MessageBuilder.BuildGenericMessageReq {}
        let sender = fromMaybe smsCfg.sender mbSender
        Sms.sendSMS driver.merchantId merchantOpCityId (Sms.SendSMSReq message phoneNumber sender templateId)
          >>= Sms.checkSmsResult
      WHATSAPP -> do
        merchantMessage <-
          QMM.findByMerchantOpCityIdAndMessageKeyVehicleCategory merchantOpCityId mkey Nothing Nothing
            >>= fromMaybeM (MerchantMessageNotFound merchantOpCityId.getId (show mkey))
        result <- Whatsapp.whatsAppSendMessageWithTemplateIdAPI driver.merchantId merchantOpCityId (Whatsapp.SendWhatsAppMessageWithTemplateIdApIReq phoneNumber merchantMessage.templateId [(Just $ fromMaybe "XXXXX" vehicleId), (Just timeStamp), mbReason] Nothing (Just merchantMessage.containsUrlButton)) -- Accepts at most 7 variables using GupShup
        when (result._response.status /= "success") $ throwError (InternalError "Unable to send Whatsapp message via dashboard")
      _ -> pure ()

appendPlusInMobileCountryCode :: Maybe Text -> Maybe Text
appendPlusInMobileCountryCode = fmap (\code -> if "+" `T.isPrefixOf` code then code else "+" <> code)

castStatus :: DriverFeeStatus -> Common.DriverFeeStatus
castStatus status = case status of -- only PENDING and OVERDUE possible
  ONGOING -> Common.ONGOING
  PAYMENT_PENDING -> Common.PAYMENT_PENDING
  PAYMENT_OVERDUE -> Common.PAYMENT_OVERDUE
  CLEARED -> Common.CLEARED
  EXEMPTED -> Common.EXEMPTED
  COLLECTED_CASH -> Common.COLLECTED_CASH
  INACTIVE -> Common.INACTIVE
  CLEARED_BY_YATRI_COINS -> Common.CLEARED_BY_YATRI_COINS
  MANUAL_REVIEW_NEEDED -> Common.MANUAL_REVIEW_NEEDED
  REFUND_PENDING -> Common.REFUND_PENDING
  REFUNDED -> Common.REFUNDED
  REFUND_FAILED -> Common.REFUND_FAILED
  REFUND_MANUAL_REVIEW_REQUIRED -> Common.REFUND_MANUAL_REVIEW_REQUIRED
  ONE_TIME_SECURITY_ADJUSTED -> Common.ONE_TIME_SECURITY_ADJUSTED
  SETTLED -> Common.SETTLED
  IN_DISPUTE_WINDOW -> Common.IN_DISPUTE_WINDOW
  ADDED_TO_INVOICE -> Common.ADDED_TO_INVOICE

checkFleetOwnerVerification :: Text -> Maybe Bool -> Flow ()
checkFleetOwnerVerification personId mbEnabledCheck = do
  when (mbEnabledCheck == Just True) $ do
    fleetOwnerInfo <- QFI.findByPrimaryKey (Id personId) >>= fromMaybeM (InvalidRequest $ "Fleet owner does not exist " <> personId)
    unless fleetOwnerInfo.enabled $ throwError (InvalidRequest "Fleet owner is not enabled")

checkFleetOwnerRole :: DP.Role -> Bool
checkFleetOwnerRole role = role `elem` [DP.FLEET_OWNER, DP.FLEET_BUSINESS]

castDashboardVehicleVariantToDomain :: Common.VehicleVariant -> DV.VehicleVariant
castDashboardVehicleVariantToDomain = \case
  Common.SUV -> DV.SUV
  Common.HATCHBACK -> DV.HATCHBACK
  Common.SEDAN -> DV.SEDAN
  Common.AUTO_RICKSHAW -> DV.AUTO_RICKSHAW
  Common.AUTO_PLUS -> DV.AUTO_PLUS
  Common.TAXI -> DV.TAXI
  Common.TAXI_PLUS -> DV.TAXI_PLUS
  Common.PREMIUM_SEDAN -> DV.PREMIUM_SEDAN
  Common.BLACK -> DV.BLACK
  Common.BLACK_XL -> DV.BLACK_XL
  Common.BIKE -> DV.BIKE
  Common.AMBULANCE_TAXI -> DV.AMBULANCE_TAXI
  Common.AMBULANCE_TAXI_OXY -> DV.AMBULANCE_TAXI_OXY
  Common.AMBULANCE_AC -> DV.AMBULANCE_AC
  Common.AMBULANCE_AC_OXY -> DV.AMBULANCE_AC_OXY
  Common.AMBULANCE_VENTILATOR -> DV.AMBULANCE_VENTILATOR
  Common.SUV_PLUS -> DV.SUV_PLUS
  Common.DELIVERY_BIKE -> DV.DELIVERY_BIKE
  Common.DELIVERY_LIGHT_GOODS_VEHICLE -> DV.DELIVERY_LIGHT_GOODS_VEHICLE
  Common.DELIVERY_TRUCK_MINI -> DV.DELIVERY_TRUCK_MINI
  Common.DELIVERY_TRUCK_SMALL -> DV.DELIVERY_TRUCK_SMALL
  Common.DELIVERY_TRUCK_MEDIUM -> DV.DELIVERY_TRUCK_MEDIUM
  Common.DELIVERY_TRUCK_LARGE -> DV.DELIVERY_TRUCK_LARGE
  Common.DELIVERY_TRUCK_ULTRA_LARGE -> DV.DELIVERY_TRUCK_ULTRA_LARGE
  Common.BUS_NON_AC -> DV.BUS_NON_AC
  Common.BUS_AC -> DV.BUS_AC
  Common.BOAT -> DV.BOAT
  Common.HERITAGE_CAB -> DV.HERITAGE_CAB
  Common.EV_AUTO_RICKSHAW -> DV.EV_AUTO_RICKSHAW
  Common.VIP_ESCORT -> DV.VIP_ESCORT
  Common.VIP_OFFICER -> DV.VIP_OFFICER
  Common.AC_PRIORITY -> DV.AC_PRIORITY
  Common.BIKE_PLUS -> DV.BIKE_PLUS
  Common.E_RICKSHAW -> DV.E_RICKSHAW
