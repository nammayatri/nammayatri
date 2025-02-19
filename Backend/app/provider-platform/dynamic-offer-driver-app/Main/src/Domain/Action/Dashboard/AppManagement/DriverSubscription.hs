{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Dashboard.AppManagement.DriverSubscription
  ( postDriverSubscriptionSendSms,
    postDriverSubscriptionUpdateDriverFeeAndInvoiceInfo,
  )
where

import qualified API.Types.Dashboard.AppManagement.DriverSubscription as DriverSubscription
import qualified "this" API.Types.Dashboard.RideBooking.Driver as Common
import Control.Applicative ((<|>))
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Time hiding (getCurrentTime, secondsToNominalDiffTime)
import qualified Domain.Action.Dashboard.Common as DCommon
import Domain.Types.DriverFee as DDF
import qualified Domain.Types.Invoice as INV
import qualified Domain.Types.Merchant as DM
import Domain.Types.MerchantMessage (MediaChannel (..))
import qualified Domain.Types.Message as Domain
import qualified Domain.Types.Person as DP
import Domain.Types.Plan
import Domain.Types.TransporterConfig
import Environment
import Kernel.Beam.Functions as B
import Kernel.External.Encryption
import Kernel.External.Types (Language (..))
import Kernel.Prelude
import Kernel.Streaming.Kafka.Producer (produceMessage)
import Kernel.Types.APISuccess (APISuccess (Success))
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import qualified Kernel.Types.SlidingWindowCounters as SWC
import Kernel.Utils.Common
import qualified Kernel.Utils.SlidingWindowCounters as SWC
import qualified SharedLogic.DriverFee as SLDriverFee
import SharedLogic.Merchant (findMerchantByShortId)
import qualified SharedLogic.Merchant as SMerchant
import qualified SharedLogic.MessageBuilder as MessageBuilder
import Storage.Beam.Yudhishthira ()
import qualified Storage.Cac.TransporterConfig as CTC
import qualified Storage.CachedQueries.Merchant.MerchantMessage as QMM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Merchant.Overlay as CMP
import qualified Storage.Queries.DriverFee as QDF
import qualified Storage.Queries.DriverInformation as QDriverInfo
import qualified Storage.Queries.Invoice as QINV
import qualified Storage.Queries.Message as MQuery
import qualified Storage.Queries.MessageTranslation as MTQuery
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Vehicle as QVehicle
import Tools.Error
import qualified Tools.Notifications as TN
import qualified Tools.SMS as Sms
import Tools.Whatsapp as Whatsapp

postDriverSubscriptionSendSms :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Text -> DriverSubscription.SendSmsReq -> Flow APISuccess
postDriverSubscriptionSendSms merchantShortId opCity driverId volunteerId _req@DriverSubscription.SendSmsReq {..} = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  -- limit checking
  transporterConfig <- CTC.findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  void $ checkIfVolunteerSMSSendingLimitExceeded volunteerId transporterConfig.volunteerSmsSendingLimit channel
  void $ checkIfDriverSMSReceivingLimitExceeded driverId.getId transporterConfig.driverSmsReceivingLimit channel

  let personId = cast @Common.Driver @DP.Person driverId
  mbVehicle <- QVehicle.findById personId
  let mbVehicleCategory = mbVehicle >>= (.category)
  driver <- B.runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  -- merchant access check
  unless (merchant.id == driver.merchantId && merchantOpCityId == driver.merchantOperatingCityId) $ throwError (PersonDoesNotExist personId.getId)
  smsCfg <- asks (.smsCfg)
  mobileNumber <- mapM decrypt driver.mobileNumber >>= fromMaybeM (PersonFieldNotPresent "mobileNumber")
  countryCode <- driver.mobileCountryCode & fromMaybeM (PersonFieldNotPresent "mobileCountryCode")
  let phoneNumber = countryCode <> mobileNumber
  withLogTag ("personId_" <> personId.getId) $ do
    case channel of
      SMS -> do
        mkey <- fromMaybeM (InvalidRequest "Message Key field is required for channel : SMS") messageKey --whenJust messageKey $ \mkey -> do
        (mbSender, message) <- MessageBuilder.buildGenericMessage merchantOpCityId mkey mbVehicleCategory MessageBuilder.BuildGenericMessageReq {}
        let sender = fromMaybe smsCfg.sender mbSender
        Sms.sendSMS driver.merchantId merchantOpCityId (Sms.SendSMSReq message phoneNumber sender)
          >>= Sms.checkSmsResult
      WHATSAPP -> do
        mkey <- fromMaybeM (InvalidRequest "Message Key field is required for channel : WHATSAPP") messageKey -- whenJust messageKey $ \mkey -> do
        merchantMessage <-
          QMM.findByMerchantOpCityIdAndMessageKeyVehicleCategory merchantOpCityId mkey mbVehicleCategory Nothing
            >>= fromMaybeM (MerchantMessageNotFound merchantOpCityId.getId (show mkey))
        let jsonData = merchantMessage.jsonData
        result <- Whatsapp.whatsAppSendMessageWithTemplateIdAPI driver.merchantId merchantOpCityId (Whatsapp.SendWhatsAppMessageWithTemplateIdApIReq phoneNumber merchantMessage.templateId jsonData.var1 jsonData.var2 jsonData.var3 Nothing Nothing Nothing Nothing Nothing (Just merchantMessage.containsUrlButton))
        when (result._response.status /= "success") $ throwError (InternalError "Unable to send Whatsapp message via dashboard")
      OVERLAY -> do
        oKey <- fromMaybeM (InvalidRequest "Overlay Key field is required for channel : OVERLAY") overlayKey --whenJust overlayKey $ \oKey -> do
        manualDues <- getManualDues personId transporterConfig.timeDiffFromUtc transporterConfig.driverFeeOverlaySendingTimeLimitInDays
        overlay <- CMP.findByMerchantOpCityIdPNKeyLangaugeUdfVehicleCategory merchantOpCityId oKey (fromMaybe ENGLISH driver.language) Nothing mbVehicleCategory >>= fromMaybeM (OverlayKeyNotFound oKey)
        let okButtonText = T.replace (templateText "dueAmount") (show manualDues) <$> overlay.okButtonText
        let description = T.replace (templateText "dueAmount") (show manualDues) <$> overlay.description
        let overlay' = overlay{okButtonText, description}
        TN.sendOverlay merchantOpCityId driver $ TN.mkOverlayReq overlay'
      ALERT -> do
        _mId <- fromMaybeM (InvalidRequest "Message Id field is required for channel : ALERT") messageId -- whenJust messageId $ \_mId -> do
        topicName <- asks (.broadcastMessageTopic)
        message <- B.runInReplica $ MQuery.findById (Id _mId) >>= fromMaybeM (InvalidRequest "Message Not Found")
        msg <- createMessageLanguageDict message
        produceMessage (topicName, Just (encodeUtf8 $ getId driverId)) msg
  -- if the message is sent successfuly then increment the count of both volunteer and driver
  void $ incrementVolunteerSMSSendingCount volunteerId channel
  void $ incrementDriverSMSReceivingCount driverId.getId channel
  pure Success
  where
    createMessageLanguageDict :: Domain.RawMessage -> Flow Domain.MessageDict
    createMessageLanguageDict message = do
      translations <- B.runInReplica $ MTQuery.findByMessageId message.id
      pure $ Domain.MessageDict message (M.fromList $ map (addTranslation message) translations)

    addTranslation Domain.RawMessage {..} trans =
      (show trans.language, Domain.RawMessage {title = trans.title, description = trans.description, shortDescription = trans.shortDescription, label = trans.label, ..})

    getManualDues personId timeDiffFromUtc driverFeeOverlaySendingTimeLimitInDays = do
      windowEndTime <- getLocalCurrentTime timeDiffFromUtc
      let windowStartTime = addUTCTime (-1 * fromIntegral driverFeeOverlaySendingTimeLimitInDays * 86400) (UTCTime (utctDay windowEndTime) (secondsToDiffTime 0))
      pendingDriverFees <- QDF.findAllOverdueDriverFeeByDriverIdForServiceName personId YATRI_SUBSCRIPTION
      let filteredDriverFees = filter (\driverFee -> driverFee.startTime >= windowStartTime) pendingDriverFees
      return $
        if null filteredDriverFees
          then 0
          else sum $ map (\dueInvoice -> SLDriverFee.roundToHalf dueInvoice.currency (dueInvoice.govtCharges + dueInvoice.platformFee.fee + dueInvoice.platformFee.cgst + dueInvoice.platformFee.sgst)) pendingDriverFees

    templateText txt = "{#" <> txt <> "#}"

checkIfVolunteerSMSSendingLimitExceeded :: (CacheFlow m r, MonadFlow m) => Text -> Maybe DashboardMediaSendingLimit -> MediaChannel -> m ()
checkIfVolunteerSMSSendingLimitExceeded volunteerId limitConfig channel = do
  let limit = case limitConfig of
        Nothing -> 500
        Just config -> getLimitAccordingToChannel config channel
  (currentLimit :: Int) <- fromIntegral <$> SWC.getCurrentWindowCount (mkVolunteerSMSSendingLimitKey volunteerId channel) windowLimit
  when (currentLimit >= limit) $ throwError (VolunteerMessageSendingLimitExceeded (show channel)) -- the limit is counted from 0

checkIfDriverSMSReceivingLimitExceeded :: (CacheFlow m r, MonadFlow m) => Text -> Maybe DashboardMediaSendingLimit -> MediaChannel -> m ()
checkIfDriverSMSReceivingLimitExceeded driverId limitConfig channel = do
  let limit = case limitConfig of
        Nothing -> 10
        Just config -> getLimitAccordingToChannel config channel
  (currentLimit :: Int) <- fromIntegral <$> SWC.getCurrentWindowCount (mkDriverSMSRecevingLimitKey driverId channel) windowLimit
  when (currentLimit >= limit) $ throwError (DriverMessageReceivingLimitExceeded (show channel)) -- the limit is counted from 0

getLimitAccordingToChannel :: DashboardMediaSendingLimit -> MediaChannel -> Int
getLimitAccordingToChannel config channel =
  case channel of
    SMS -> config.sms
    WHATSAPP -> config.whatsapp
    OVERLAY -> config.overlay
    ALERT -> config.alert

incrementVolunteerSMSSendingCount :: (CacheFlow m r, MonadFlow m) => Text -> MediaChannel -> m ()
incrementVolunteerSMSSendingCount volunteerId channel = SWC.incrementWindowCount (mkVolunteerSMSSendingLimitKey volunteerId channel) windowLimit

incrementDriverSMSReceivingCount :: (CacheFlow m r, MonadFlow m) => Text -> MediaChannel -> m ()
incrementDriverSMSReceivingCount driverId channel = SWC.incrementWindowCount (mkDriverSMSRecevingLimitKey driverId channel) windowLimit

mkVolunteerSMSSendingLimitKey :: Text -> MediaChannel -> Text
mkVolunteerSMSSendingLimitKey volunteerId channel = "Dashboard:VolunteerId-" <> volunteerId <> ":channel-" <> show channel <> ":SendSMS:HitCount"

mkDriverSMSRecevingLimitKey :: Text -> MediaChannel -> Text
mkDriverSMSRecevingLimitKey driverId channel = "Dashboard:DriverId-" <> driverId <> ":channel-" <> show channel <> ":SendSMS:ReceiveCount"

windowLimit :: SWC.SlidingWindowOptions
windowLimit = SWC.SlidingWindowOptions 24 SWC.Hours

---------------------------------------------------------------------
postDriverSubscriptionUpdateDriverFeeAndInvoiceInfo ::
  ShortId DM.Merchant ->
  Context.City ->
  Id Common.Driver ->
  Common.ServiceNames ->
  DriverSubscription.SubscriptionDriverFeesAndInvoicesToUpdate ->
  Flow DriverSubscription.SubscriptionDriverFeesAndInvoicesToUpdate
postDriverSubscriptionUpdateDriverFeeAndInvoiceInfo merchantShortId opCity driverId serviceName' DriverSubscription.SubscriptionDriverFeesAndInvoicesToUpdate {..} = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  now <- getCurrentTime
  let serviceName = DCommon.mapServiceName serviceName'
  let personId = cast @Common.Driver @DP.Person driverId
  driver <- B.runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  unless (merchant.id == driver.merchantId && merchantOpCityId == driver.merchantOperatingCityId) $ throwError (PersonDoesNotExist personId.getId)
  maybe (pure ()) (`QDriverInfo.updateSubscription` personId) subscribed
  dueDriverFees <- QDF.findAllPendingAndDueDriverFeeByDriverIdForServiceName personId serviceName
  let invoicesDataToUpdate = maybe [] mapToInvoiceInfoToUpdateAfterParse invoices
  mapM_ (\inv -> QINV.updateStatusAndTypeByMbdriverFeeIdAndInvoiceId inv.invoiceId inv.invoiceStatus Nothing inv.driverFeeId) invoicesDataToUpdate
  allDriverFeeByIds <- QDF.findAllByDriverFeeIds (maybe [] (map (\df -> cast (Id df.driverFeeId))) driverFees)
  let reqMkDuesToAmount = (mkDuesToAmountWithCurrency <&> (.amount)) <|> mkDuesToAmount
  currency <- SMerchant.getCurrencyByMerchantOpCity merchantOpCityId
  SMerchant.checkCurrencies currency $ do
    let driverFeesFields = flip (maybe []) driverFees $
          concatMap $ \driverFees' ->
            [ driverFees'.platformFeeWithCurrency,
              driverFees'.sgstWithCurrency,
              driverFees'.cgstWithCurrency
            ]
    mkDuesToAmountWithCurrency : driverFeesFields
  if isJust reqMkDuesToAmount
    then do
      let amount = maybe 0.0 (/ (fromIntegral $ length dueDriverFees)) reqMkDuesToAmount
      mapM_ (\fee -> QDF.resetFee fee.id 0 (PlatformFee {fee = amount, cgst = 0.0, sgst = 0.0, currency = fee.currency}) Nothing Nothing now) dueDriverFees
      return $ mkResponse dueDriverFees
    else do
      maybe (pure ()) (updateAccordingToProvidedFeeState currency now) driverFees
      return $ mkResponse allDriverFeeByIds
  where
    mkResponse driverFees' =
      DriverSubscription.SubscriptionDriverFeesAndInvoicesToUpdate
        { driverFees = Just $ mapToDriverFeeToUpdate driverFees',
          invoices = Nothing,
          mkDuesToAmount = Nothing,
          mkDuesToAmountWithCurrency = Nothing,
          subscribed = Nothing
        }
    mapToInvoiceInfoToUpdateAfterParse =
      map
        ( \invData -> do
            let mbInvoiceStatus = (\invs -> readMaybe (T.unpack invs) :: (Maybe INV.InvoiceStatus)) =<< invData.invoiceStatus
            InvoiceInfoToUpdateAfterParse
              { invoiceId = cast (Id invData.invoiceId),
                driverFeeId = cast . Id <$> invData.driverFeeId,
                invoiceStatus = mbInvoiceStatus
              }
        )
    mapToDriverFeeToUpdate =
      map
        ( \dfee ->
            DriverSubscription.DriverFeeInfoToUpdate
              { driverFeeId = dfee.id.getId,
                mkManualDue = Nothing,
                mkAutoPayDue = Nothing,
                mkCleared = Nothing,
                platformFee = Just $ dfee.platformFee.fee,
                cgst = Just dfee.platformFee.cgst,
                sgst = Just dfee.platformFee.sgst,
                platformFeeWithCurrency = Just $ PriceAPIEntity dfee.platformFee.fee dfee.currency,
                cgstWithCurrency = Just $ PriceAPIEntity dfee.platformFee.cgst dfee.currency,
                sgstWithCurrency = Just $ PriceAPIEntity dfee.platformFee.sgst dfee.currency
              }
        )
    updateAccordingToProvidedFeeState currency now =
      mapM_
        ( \fee -> do
            let id = cast (Id fee.driverFeeId)
                platFormFee' = fromMaybe 0 ((fee.platformFeeWithCurrency <&> (.amount)) <|> fee.platformFee)
                sgst = fromMaybe 0 ((fee.sgstWithCurrency <&> (.amount)) <|> fee.sgst)
                cgst = fromMaybe 0 ((fee.cgstWithCurrency <&> (.amount)) <|> fee.cgst)
                platFormFee = PlatformFee {fee = platFormFee', sgst, cgst, currency}
            QDF.resetFee id 0 platFormFee Nothing Nothing now
            when (fee.mkManualDue == Just True) $ do QDF.updateAutoPayToManual id
            when (fee.mkAutoPayDue == Just True && fee.mkManualDue `elem` [Nothing, Just False]) $ do QDF.updateManualToAutoPay id
        )

data InvoiceInfoToUpdateAfterParse = InvoiceInfoToUpdateAfterParse
  { invoiceId :: Id INV.Invoice,
    driverFeeId :: Maybe (Id DriverFee),
    invoiceStatus :: Maybe INV.InvoiceStatus
  }
