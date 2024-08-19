{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Dashboard.Common
  ( mobileIndianCode,
    mapServiceName,
    castVerificationStatus,
    castVehicleVariantDashboard,
    notifyYatriRentalEventsToDriver,
    runVerifyRCFlow,
    appendPlusInMobileCountryCode,
  )
where

import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Fleet.Driver as Common
import qualified Data.Text as T
import Data.Time hiding (getCurrentTime, secondsToNominalDiffTime)
import qualified Domain.Action.UI.DriverOnboarding.VehicleRegistrationCertificate as DomainRC
import qualified Domain.Types.Merchant as DM
import Domain.Types.MerchantMessage (MediaChannel (..), MessageKey (..))
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import Domain.Types.Plan
import Domain.Types.TransporterConfig
import qualified Domain.Types.VehicleVariant as DV
import Environment
import Kernel.Beam.Functions as B
import Kernel.External.Encryption (decrypt)
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

mapServiceName :: Common.ServiceNames -> ServiceNames
mapServiceName common = case common of
  Common.YATRI_SUBSCRIPTION -> YATRI_SUBSCRIPTION
  Common.YATRI_RENTAL -> YATRI_RENTAL

mobileIndianCode :: Text
mobileIndianCode = "+91"

appendPlusInMobileCountryCode :: Maybe Text -> Maybe Text
appendPlusInMobileCountryCode = fmap (\code -> if "+" `T.isPrefixOf` code then code else "+" <> code)

castVerificationStatus :: Documents.VerificationStatus -> Common.VerificationStatus
castVerificationStatus = \case
  Documents.PENDING -> Common.PENDING
  Documents.VALID -> Common.VALID
  Documents.MANUAL_VERIFICATION_REQUIRED -> Common.MANUAL_VERIFICATION_REQUIRED
  Documents.INVALID -> Common.INVALID
  Documents.UNAUTHORIZED -> Common.UNAUTHORIZED

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
  _ -> Nothing

notifyYatriRentalEventsToDriver :: Text -> MessageKey -> Id DP.Person -> TransporterConfig -> Maybe Text -> MediaChannel -> Flow ()
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
        (mbSender, message) <- MessageBuilder.buildGenericMessage merchantOpCityId mkey MessageBuilder.BuildGenericMessageReq {}
        let sender = fromMaybe smsCfg.sender mbSender
        Sms.sendSMS driver.merchantId merchantOpCityId (Sms.SendSMSReq message phoneNumber sender)
          >>= Sms.checkSmsResult
      WHATSAPP -> do
        merchantMessage <-
          QMM.findByMerchantOpCityIdAndMessageKey merchantOpCityId mkey
            >>= fromMaybeM (MerchantMessageNotFound merchantOpCityId.getId (show mkey))
        result <- Whatsapp.whatsAppSendMessageWithTemplateIdAPI driver.merchantId merchantOpCityId (Whatsapp.SendWhatsAppMessageWithTemplateIdApIReq phoneNumber merchantMessage.templateId (Just vehicleId) (Just timeStamp) mbReason Nothing (Just merchantMessage.containsUrlButton))
        when (result._response.status /= "success") $ throwError (InternalError "Unable to send Whatsapp message via dashboard")
      _ -> pure ()
