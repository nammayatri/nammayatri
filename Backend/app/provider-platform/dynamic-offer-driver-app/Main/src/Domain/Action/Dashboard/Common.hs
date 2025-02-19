module Domain.Action.Dashboard.Common
  ( mobileIndianCode,
    mapServiceName,
    castVerificationStatus,
    castVehicleVariantDashboard,
    notifyYatriRentalEventsToDriver,
    runVerifyRCFlow,
    appendPlusInMobileCountryCode,
    castStatus,
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
import qualified Storage.Queries.Person as QPerson
import Tools.Error
import qualified Tools.SMS as Sms
import Tools.Whatsapp as Whatsapp

mobileIndianCode :: Text
mobileIndianCode = "+91"

mapServiceName :: Common.ServiceNames -> ServiceNames
mapServiceName common = case common of
  Common.YATRI_SUBSCRIPTION -> YATRI_SUBSCRIPTION
  Common.YATRI_RENTAL -> YATRI_RENTAL

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
  _ -> Nothing

runVerifyRCFlow :: Id DP.Person -> DM.Merchant -> Id DMOC.MerchantOperatingCity -> Context.City -> Common.AddVehicleReq -> Bool -> Flow ()
runVerifyRCFlow personId merchant merchantOpCityId operatingCity req isFleet = do
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
            multipleRC = Nothing,
            vehicleDetails = Nothing,
            vehicleCategory = req.vehicleCategory
          }
  void $ DomainRC.verifyRC (not isFleet) (Just merchant) (personId, merchant.id, merchantOpCityId) rcReq

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
        (mbSender, message) <- MessageBuilder.buildGenericMessage merchantOpCityId mkey Nothing MessageBuilder.BuildGenericMessageReq {}
        let sender = fromMaybe smsCfg.sender mbSender
        Sms.sendSMS driver.merchantId merchantOpCityId (Sms.SendSMSReq message phoneNumber sender)
          >>= Sms.checkSmsResult
      WHATSAPP -> do
        merchantMessage <-
          QMM.findByMerchantOpCityIdAndMessageKeyVehicleCategory merchantOpCityId mkey Nothing Nothing
            >>= fromMaybeM (MerchantMessageNotFound merchantOpCityId.getId (show mkey))
        result <- Whatsapp.whatsAppSendMessageWithTemplateIdAPI driver.merchantId merchantOpCityId (Whatsapp.SendWhatsAppMessageWithTemplateIdApIReq phoneNumber merchantMessage.templateId (Just $ fromMaybe "XXXXX" vehicleId) (Just timeStamp) mbReason Nothing Nothing Nothing Nothing Nothing (Just merchantMessage.containsUrlButton))
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
