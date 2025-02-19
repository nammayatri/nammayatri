{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Payment where

import Data.List (groupBy, sortBy)
import Data.Time (UTCTime (UTCTime), secondsToDiffTime, utctDay)
import Domain.Types.DriverFee
import qualified Domain.Types.Invoice as INV
import qualified Domain.Types.Merchant as DM
import Domain.Types.MerchantMessage as MessageKey
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.MerchantServiceConfig as DMSC
import qualified Domain.Types.Person as DP
import Domain.Types.Plan as DP
import qualified Domain.Types.VendorFee as VF
import qualified Kernel.Beam.Functions as B
import Kernel.External.Encryption
import qualified Kernel.External.Payment.Interface as Payment
import Kernel.External.Payment.Interface.Types
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude
import Kernel.Sms.Config (SmsConfig)
import Kernel.Storage.Esqueleto as Esq hiding (Value, groupBy, on)
import Kernel.Storage.Hedis as Hedis
import Kernel.Types.Common hiding (id)
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Payment.Domain.Action as DPayment
import qualified Lib.Payment.Domain.Types.Common as DPayment
import qualified Lib.Payment.Domain.Types.PaymentOrder as DOrder
import SharedLogic.DriverFee (roundToHalf)
import qualified SharedLogic.MessageBuilder as MessageBuilder
import Storage.Beam.Payment ()
import Storage.Cac.TransporterConfig as SCTC
import qualified Storage.CachedQueries.Merchant.MerchantMessage as QMM
import qualified Storage.CachedQueries.SubscriptionConfig as CQSC
import qualified Storage.Queries.Invoice as QIN
import qualified Storage.Queries.Person as QP
import Tools.Error
import Tools.Metrics
import qualified Tools.Payment as TPayment
import qualified Tools.SMS as Sms
import Tools.Whatsapp as Whatsapp

data MandateOrder = MandateOrder
  { maxAmount :: HighPrecMoney,
    _type :: MandateType,
    frequency :: MandateFrequency,
    mandateStartDate :: Text,
    mandateEndDate :: Text
  }

createOrder ::
  ( CacheFlow m r,
    EsqDBReplicaFlow m r,
    EsqDBFlow m r,
    EncFlow m r,
    CoreMetrics m,
    MonadFlow m
  ) =>
  (Id DP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  DMSC.ServiceName ->
  ([DriverFee], [DriverFee]) ->
  Maybe MandateOrder ->
  INV.InvoicePaymentMode ->
  Maybe (Id INV.Invoice, Text) ->
  [VF.VendorFee] ->
  Maybe DeepLinkData ->
  Bool ->
  m (CreateOrderResp, Id DOrder.PaymentOrder)
createOrder (driverId, merchantId, opCity) serviceName (driverFees, driverFeesToAddOnExpiry) mbMandateOrder invoicePaymentMode existingInvoice vendorFees mbDeepLinkData splitEnabled = do
  mapM_ (\driverFee -> when (driverFee.status `elem` [CLEARED, EXEMPTED, COLLECTED_CASH]) $ throwError (DriverFeeAlreadySettled driverFee.id.getId)) driverFees
  mapM_ (\driverFee -> when (driverFee.status `elem` [INACTIVE, ONGOING]) $ throwError (DriverFeeNotInUse driverFee.id.getId)) driverFees
  driver <- B.runInReplica $ QP.findById driverId >>= fromMaybeM (PersonNotFound $ getId driverId)
  unless (driver.id == driverId) $ throwError NotAnExecutor
  driverPhone <- driver.mobileNumber & fromMaybeM (PersonFieldNotPresent "mobileNumber") >>= decrypt
  genInvoiceId <- generateGUID
  genShortInvoiceId <- generateShortId
  now <- getCurrentTime
  let driverEmail = fromMaybe "test@juspay.in" driver.email
      (invoiceId, invoiceShortId) = fromMaybe (genInvoiceId, genShortInvoiceId.getShortId) existingInvoice
      amount = sum $ (\pendingFees -> roundToHalf pendingFees.currency (pendingFees.govtCharges + pendingFees.platformFee.fee + pendingFees.platformFee.cgst + pendingFees.platformFee.sgst)) <$> driverFees
      invoices = mkInvoiceAgainstDriverFee invoiceId.getId invoiceShortId now (mbMandateOrder <&> (.maxAmount)) invoicePaymentMode <$> driverFees
  splitSettlementDetails <- if splitEnabled then mkSplitSettlementDetails vendorFees amount else pure Nothing
  logInfo $ "split details: " <> show splitSettlementDetails
  when (amount <= 0) $ throwError (InternalError "Invalid Amount :- should be greater than 0")
  unless (isJust existingInvoice) $ QIN.createMany invoices
  let createOrderReq =
        CreateOrderReq
          { orderId = invoiceId.getId,
            orderShortId = invoiceShortId,
            amount = amount,
            customerId = driver.id.getId,
            customerEmail = driverEmail,
            customerPhone = driverPhone,
            customerFirstName = Just driver.firstName,
            customerLastName = driver.lastName,
            createMandate = mbMandateOrder <&> (._type),
            mandateMaxAmount = mbMandateOrder <&> (.maxAmount),
            mandateFrequency = mbMandateOrder <&> (.frequency),
            mandateEndDate = mbMandateOrder <&> (.mandateEndDate),
            mandateStartDate = mbMandateOrder <&> (.mandateStartDate),
            optionsGetUpiDeepLinks = mbDeepLinkData >>= (.sendDeepLink),
            metadataExpiryInMins = mbDeepLinkData >>= (.expiryTimeInMinutes),
            splitSettlementDetails = splitSettlementDetails,
            metadataGatewayReferenceId = Nothing --- assigned in shared kernel
          }
  let commonMerchantId = cast @DM.Merchant @DPayment.Merchant merchantId
      commonPersonId = cast @DP.Person @DPayment.Person driver.id
      createOrderCall = TPayment.createOrder merchantId opCity serviceName -- api call
  mCreateOrderRes <- DPayment.createOrderService commonMerchantId (Just $ cast opCity) commonPersonId createOrderReq createOrderCall
  case mCreateOrderRes of
    Just createOrderRes -> return (createOrderRes, cast invoiceId)
    Nothing -> do
      QIN.updateInvoiceStatusByInvoiceId INV.EXPIRED invoiceId
      createOrder (driverId, merchantId, opCity) serviceName (driverFees <> driverFeesToAddOnExpiry, []) mbMandateOrder invoicePaymentMode Nothing vendorFees mbDeepLinkData splitEnabled -- call same function with no existing order

mkSplitSettlementDetails :: (MonadFlow m) => [VF.VendorFee] -> HighPrecMoney -> m (Maybe SplitSettlementDetails)
mkSplitSettlementDetails vendorFees totalAmount = do
  let sortedVendorFees = sortBy (compare `on` VF.vendorId) vendorFees
      groupedVendorFees = groupBy ((==) `on` VF.vendorId) sortedVendorFees
      mbVendorSplits = map computeSplit groupedVendorFees
      vendorSplits = catMaybes mbVendorSplits
  let totalVendorAmount = roundToTwoDecimalPlaces $ sum $ map (\Split {amount} -> amount) vendorSplits
      marketplaceAmount = roundToTwoDecimalPlaces (totalAmount - totalVendorAmount)
  logInfo $ "totalVendorAmount: " <> show totalVendorAmount <> " marketplaceAmount: " <> show marketplaceAmount <> " totalAmount: " <> show totalAmount
  when (marketplaceAmount < 0) $ do
    logError $ "Marketplace amount is negative: " <> show marketplaceAmount <> " for vendorFees: " <> show vendorFees <> "totalVendorAmount: " <> show totalVendorAmount <> " totalAmount: " <> show totalAmount
    throwError (InternalError "Marketplace amount is negative")
  return $
    Just $
      SplitSettlementDetails
        { marketplace = Marketplace marketplaceAmount,
          mdrBorneBy = ALL,
          vendor = Vendor vendorSplits
        }
  where
    computeSplit feesForVendor =
      case feesForVendor of
        [] -> Nothing
        (firstFee : _) ->
          Just $
            Split
              { amount = roundToTwoDecimalPlaces $ sum $ map (\fee -> VF.amount fee) feesForVendor,
                merchantCommission = 0,
                subMid = firstFee.vendorId
              }

roundToTwoDecimalPlaces :: HighPrecMoney -> HighPrecMoney
roundToTwoDecimalPlaces x = fromIntegral (round (x * 100) :: Integer) / 100

roundVendorFee :: VF.VendorFee -> VF.VendorFee
roundVendorFee vf = vf {VF.amount = roundToTwoDecimalPlaces (VF.amount vf)}

mkInvoiceAgainstDriverFee :: Text -> Text -> UTCTime -> Maybe HighPrecMoney -> INV.InvoicePaymentMode -> DriverFee -> INV.Invoice
mkInvoiceAgainstDriverFee id shortId now maxMandateAmount paymentMode driverFee =
  INV.Invoice
    { id = Id id,
      invoiceShortId = shortId,
      driverFeeId = driverFee.id,
      invoiceStatus = INV.ACTIVE_INVOICE,
      driverId = driverFee.driverId,
      maxMandateAmount,
      paymentMode,
      bankErrorCode = Nothing,
      bankErrorMessage = Nothing,
      bankErrorUpdatedAt = Nothing,
      lastStatusCheckedAt = Nothing,
      serviceName = driverFee.serviceName,
      merchantId = Just driverFee.merchantId,
      merchantOperatingCityId = driverFee.merchantOperatingCityId,
      updatedAt = now,
      createdAt = now
    }

offerListCache :: (MonadFlow m, ServiceFlow m r) => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> DP.ServiceNames -> Payment.OfferListReq -> m Payment.OfferListResp
offerListCache merchantId merchantOpCityId serviceName req = do
  transporterConfig <- SCTC.findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  subscriptionConfig <-
    CQSC.findSubscriptionConfigsByMerchantOpCityIdAndServiceName merchantOpCityId serviceName
      >>= fromMaybeM (NoSubscriptionConfigForService merchantOpCityId.getId $ show serviceName)
  if transporterConfig.useOfferListCache
    then do
      key <- makeOfferListCacheKey transporterConfig.cacheOfferListByDriverId req
      Hedis.get key >>= \case
        Just a -> return a
        Nothing -> cacheOfferListResponse transporterConfig.cacheOfferListByDriverId req /=<< TPayment.offerList merchantId merchantOpCityId subscriptionConfig.paymentServiceName req
    else TPayment.offerList merchantId merchantOpCityId subscriptionConfig.paymentServiceName req

cacheOfferListResponse :: (MonadFlow m, CacheFlow m r) => Bool -> Payment.OfferListReq -> Payment.OfferListResp -> m ()
cacheOfferListResponse includeDriverId req resp = do
  version <- getVersionKey
  key <- makeOfferListCacheKey includeDriverId req <&> (<> ":V-" <> version)
  Hedis.setExp key resp 86400
  where
    getVersionKey = do
      Hedis.get makeOfferListCacheVersionKey >>= \case
        Just a -> return a
        Nothing -> do
          versionId <- generateShortId
          Hedis.setExp makeOfferListCacheVersionKey versionId.getShortId (23 * 60 * 60)
          return versionId.getShortId

makeOfferListCacheKey :: (MonadFlow m, CacheFlow m r) => Bool -> Payment.OfferListReq -> m Text
makeOfferListCacheKey includeDriverId req = do
  case (req.customer, includeDriverId) of
    (Just customer, True) -> do
      return $
        "OfferList:CId" <> customer.customerId <> ":PId-" <> req.planId <> ":PM-" <> req.paymentMode <> ":n-"
          <> show req.numOfRides
          <> ":dt-"
          <> show (utctDay req.dutyDate)
          <> ":ft-"
          <> show (utctDay req.registrationDate)
          <> ":Listing-"
          <> maybe "N/A" parseValidityForCaching req.offerListingMetric
    _ ->
      return $
        "OfferList:PId-" <> req.planId <> ":PM-" <> req.paymentMode <> ":n-" <> show req.numOfRides <> ":dt-"
          <> show (utctDay req.dutyDate)
          <> ":ft-"
          <> show (utctDay req.registrationDate)
          <> ":Listing-"
          <> maybe "N/A" parseValidityForCaching req.offerListingMetric
  where
    parseValidityForCaching offerListingMetric' =
      case offerListingMetric' of
        LIST_BASED_ON_DATE listingDates -> show $ UTCTime (utctDay listingDates) (secondsToDiffTime 0)
        _ -> show offerListingMetric'

makeOfferListCacheVersionKey :: Text
makeOfferListCacheVersionKey = "OfferList:Version"

sendLinkTroughChannelProvided ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r, EncFlow m r, HasField "smsCfg" r SmsConfig) =>
  Maybe Payment.PaymentLinks ->
  Id DP.Person ->
  Maybe HighPrecMoney ->
  Maybe MediaChannel ->
  Bool ->
  MessageKey ->
  m ()
sendLinkTroughChannelProvided mbPaymentLink driverId mbAmount mbChannel sendDeepLink mkey = do
  driver <- QP.findById driverId >>= fromMaybeM (PersonDoesNotExist driverId.getId)
  mobileNumber <- mapM decrypt driver.mobileNumber >>= fromMaybeM (PersonFieldNotPresent "mobileNumber")
  countryCode <- driver.mobileCountryCode & fromMaybeM (PersonFieldNotPresent "mobileCountryCode")
  let phoneNumber = countryCode <> mobileNumber
      merchantOpCityId = driver.merchantOperatingCityId
      merchantId = driver.merchantId
  webPaymentLink <- getPaymentLinks mbPaymentLink driverId sendDeepLink
  case mbChannel of
    Just MessageKey.WHATSAPP -> do
      merchantMessage <- QMM.findByMerchantOpCityIdAndMessageKeyVehicleCategory merchantOpCityId mkey Nothing Nothing >>= fromMaybeM (MerchantMessageNotFound merchantOpCityId.getId (show mkey))
      let amount = show <$> mbAmount
      let whatsAppReq =
            Whatsapp.SendWhatsAppMessageWithTemplateIdApIReq
              { sendTo = phoneNumber,
                templateId = merchantMessage.templateId,
                var1 = amount,
                var2 = Nothing,
                var3 = Nothing,
                var4 = Nothing,
                var5 = Nothing,
                var6 = Nothing,
                var7 = Nothing,
                ctaButtonUrl = Just webPaymentLink,
                containsUrlButton = Just merchantMessage.containsUrlButton
              }
      result <- Whatsapp.whatsAppSendMessageWithTemplateIdAPI merchantId merchantOpCityId whatsAppReq
      when (result._response.status /= "success") $ throwError (InternalError "Unable to send Whatsapp message via dashboard")
    Just MessageKey.SMS -> do
      smsCfg <- asks (.smsCfg)
      (mbSender, message) <-
        MessageBuilder.buildSendPaymentLink merchantOpCityId $
          MessageBuilder.BuildSendPaymentLinkReq
            { paymentLink = webPaymentLink,
              amount = show (fromMaybe 0 mbAmount)
            }
      let sender = fromMaybe smsCfg.sender mbSender
      Sms.sendSMS merchantId merchantOpCityId (Sms.SendSMSReq message phoneNumber sender)
        >>= Sms.checkSmsResult
    _ -> pure ()
  return ()

getPaymentLinks :: (MonadFlow m) => Maybe Payment.PaymentLinks -> Id DP.Person -> Bool -> m Text
getPaymentLinks mbPaymentLink driverId sendDeepLink = do
  let mbDeepLink = if sendDeepLink then mbPaymentLink >>= (.deep_link) else Nothing
  case (mbPaymentLink >>= (.web), mbDeepLink) of
    (_, Just deepLink) -> return deepLink
    (Just paymentLink, Nothing) -> return $ showBaseUrl paymentLink
    (Nothing, Nothing) -> throwError $ InternalError ("No payment link found for driverId" <> show driverId.getId)

data DeepLinkData = DeepLinkData
  { sendDeepLink :: Maybe Bool,
    expiryTimeInMinutes :: Maybe Int
  }
  deriving (Generic, ToJSON, ToSchema, FromJSON, Show, Ord, Eq)
