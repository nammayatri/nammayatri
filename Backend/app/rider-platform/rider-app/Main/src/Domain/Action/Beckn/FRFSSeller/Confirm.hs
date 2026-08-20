module Domain.Action.Beckn.FRFSSeller.Confirm (handleConfirm, republish, failureMessage) where

import qualified Beckn.ACL.FRFS.Utils as ACLUtils
import qualified Beckn.ACL.FRFSSeller.OnConfirm as ACL
import qualified Beckn.ACL.FRFSSeller.OnInit as OnInitACL
import qualified Beckn.ACL.FRFSSeller.OnSearch as OnSearchACL
import qualified BecknV2.FRFS.Enums as SpecEnums
import qualified BecknV2.FRFS.Types as Spec
import qualified BecknV2.OnDemand.Enums as BecknSpec
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Domain.Types.BecknConfig as DBC
import qualified Domain.Types.FRFSQuote as DQuote
import qualified Domain.Types.FRFSQuoteCategory as DQuoteCategory
import qualified Domain.Types.FRFSQuoteCategoryType as DQuoteCategoryType
import qualified Domain.Types.FRFSRecon as Recon
import qualified Domain.Types.FRFSSearch as DSearch
import qualified Domain.Types.FRFSTicket as DTicket
import qualified Domain.Types.FRFSTicketBooking as DBooking
import qualified Domain.Types.FRFSTicketBookingStatus as DBookingStatus
import qualified Domain.Types.FRFSTicketStatus as DTicketStatus
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Environment (Flow)
import qualified ExternalBPP.ExternalAPI.CallAPI as ExternalCallAPI
import qualified ExternalBPP.ExternalAPI.Types as ExternalTypes
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.FRFSSeller.CallBAP as CallBAP
import qualified SharedLogic.FRFSSeller.Common as Common
import qualified SharedLogic.FRFSSeller.QuoteCache as QuoteCache
import qualified SharedLogic.IntegratedBPPConfig as SIBC
import qualified Storage.CachedQueries.BecknConfig as QBC
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.FRFSRecon as QRecon
import qualified Storage.Queries.FRFSTicket as QTicket
import qualified Storage.Queries.FRFSTicketBooking as QBooking
import Tools.Error
import qualified Tools.Metrics.BAPMetrics as Metrics

data ConfirmFailure
  = QuoteUnavailable
  | TermsNotAcceptable Text
  | Unprocessable Text
  | IssuanceFailed Text

failureCode :: ConfirmFailure -> Text
failureCode = \case
  QuoteUnavailable -> "30004"
  TermsNotAcceptable _ -> "30005"
  Unprocessable _ -> "31001"
  IssuanceFailed _ -> "40002"

failureMessage :: ConfirmFailure -> Text
failureMessage = \case
  QuoteUnavailable -> "The selected item is no longer available"
  TermsNotAcceptable reason -> reason
  Unprocessable reason -> reason
  IssuanceFailed reason -> reason

handleConfirm :: Text -> Spec.ConfirmReq -> Flow ()
handleConfirm operator req = do
  let ctx = req.confirmReqContext
  bapUriText <-
    ctx.contextBapUri
      & fromMaybeM (InvalidRequest "BapUri missing on confirm context")
  bapUri <- parseBaseUrl bapUriText
  transactionId <-
    ctx.contextTransactionId
      & fromMaybeM (InvalidRequest "TransactionId missing on confirm context")
  merchant <-
    CQM.findByShortId (Common.operatorMerchantShortId operator)
      >>= fromMaybeM (MerchantDoesNotExist operator)
  becknConfig <-
    QBC.findByMerchantIdDomainAndVehicle merchant.id (show SpecEnums.FRFS) BecknSpec.METRO
      >>= fromMaybeM (BecknConfigNotFound $ "merchantId:" <> merchant.id.getId <> " domain:FRFS vehicle:METRO")
  let self =
        OnSearchACL.SellerIdentity
          { subscriberId = becknConfig.subscriberId,
            subscriberUrl = showBaseUrl becknConfig.subscriberUrl
          }
  now <- getCurrentTime
  onConfirmReq <-
    confirmOrder operator transactionId merchant becknConfig req >>= \case
      Right order -> pure $ ACL.buildOnConfirmReq self now ctx order
      Left failure -> do
        logWarning $ "FRFS seller confirm rejected: " <> failureMessage failure
        pure $ ACL.buildOnConfirmErrorReq self now ctx (failureCode failure) (failureMessage failure)
  CallBAP.sendOnConfirm merchant.id becknConfig.subscriberId bapUri onConfirmReq

confirmOrder :: Text -> Text -> DM.Merchant -> DBC.BecknConfig -> Spec.ConfirmReq -> Flow (Either ConfirmFailure ACL.ConfirmedOrder)
confirmOrder operator transactionId merchant becknConfig req = runExceptT $ do
  let order = req.confirmReqMessage.confirmReqMessageOrder
  let bookingSearchId :: Id DSearch.FRFSSearch = Id (Common.sellerSearchId operator transactionId)
  existing <- lift (QBooking.findBySearchId bookingSearchId)
  case existing of
    Just booking | booking.status == DBookingStatus.CONFIRMED -> do
      lift . logInfo $ "FRFS seller confirm: republishing existing booking " <> booking.id.getId
      tickets <- lift (QTicket.findAllByTicketBookingId booking.id)
      republish operator becknConfig order.orderBilling booking tickets
    _ -> issue operator transactionId bookingSearchId merchant becknConfig req order

issue :: Text -> Text -> Id DSearch.FRFSSearch -> DM.Merchant -> DBC.BecknConfig -> Spec.ConfirmReq -> Spec.Order -> ExceptT ConfirmFailure Flow ACL.ConfirmedOrder
issue operator transactionId bookingSearchId merchant becknConfig req order = do
  item <-
    (order.orderItems >>= listToMaybe)
      & maybe (throwE (Unprocessable "Confirm carries no item")) pure
  itemId <-
    item.itemId
      & maybe (throwE (Unprocessable "Selected item has no id")) pure
  let requestedQuantity =
        fromMaybe 1 $
          item.itemQuantity
            >>= (.itemQuantitySelected)
            >>= (.itemQuantitySelectedCount)
  whenJust (Common.nonZeroBuyerFinderFee order) $ \fee ->
    throwE (TermsNotAcceptable $ "Buyer finder fee must be zero, got " <> fee)
  payment <-
    (order.orderPayments >>= listToMaybe)
      & maybe (throwE (Unprocessable "Confirm carries no payment")) pure
  paymentTxnId <-
    (payment.paymentParams >>= (.paymentParamsTransactionId))
      & maybe (throwE (Unprocessable "Confirm payment has no transaction id")) pure
  quote <-
    lift (QuoteCache.findQuote operator transactionId itemId)
      >>= maybe (throwE QuoteUnavailable) pure
  when (requestedQuantity < 1 || requestedQuantity > quote.maxTicketsPerOrder) $
    throwE (TermsNotAcceptable $ "Quantity " <> show requestedQuantity <> " outside 1.." <> show quote.maxTicketsPerOrder)
  journeyType <-
    Common.journeyTypeForItemId itemId
      & maybe (throwE (Unprocessable $ "Unrecognised item id " <> itemId)) pure
  unitPrice <-
    readMaybe (T.unpack quote.priceValue)
      & maybe (throwE (Unprocessable $ "Unreadable cached price " <> quote.priceValue)) pure
  account <- either (throwE . Unprocessable) pure (Common.settlementAccount becknConfig (Common.formatPrice (unitPrice * fromIntegral requestedQuantity)))
  cityCode <-
    (req.confirmReqContext.contextLocation >>= (.locationCity) >>= (.cityCode))
      & maybe (throwE (Unprocessable "City missing on confirm context")) pure
  city <- case A.fromJSON (A.String cityCode) of
    A.Success c -> pure c
    A.Error e -> throwE (Unprocessable $ "Unparseable city code " <> cityCode <> ": " <> show e)
  merchantOperatingCity <-
    lift (CQMOC.findByMerchantIdAndCity merchant.id city)
      >>= maybe (throwE (Unprocessable $ "No operating city " <> show city <> " for " <> operator)) pure
  integratedBPPConfig <-
    lift (SIBC.findIntegratedBPPConfig Nothing merchantOperatingCity.id BecknSpec.METRO DIBC.MULTIMODAL)

  bookingId <- lift generateGUID
  now <- lift getCurrentTime
  let validTill = addUTCTime (ticketValiditySeconds operator) now
      booking = mkBooking bookingId bookingSearchId merchant merchantOperatingCity integratedBPPConfig becknConfig quote journeyType account paymentTxnId unitPrice requestedQuantity validTill now
      quoteCategories = [mkQuoteCategory bookingId merchant merchantOperatingCity quote unitPrice requestedQuantity now]

  lift (QBooking.create booking)

  providerOrder <-
    lift (withTryCatch "frfsSeller:createOrder" (ExternalCallAPI.createOrderForJourneyType integratedBPPConfig qrTtl (Nothing, Nothing) booking quoteCategories journeyType.ticketTypeId quote.fareQuoteId)) >>= \case
      Right providerOrder -> pure providerOrder
      Left err -> do
        lift $ Metrics.incrementExternalProviderFailure integratedBPPConfig.agencyKey "createOrder" "exception"
        lift $ QBooking.updateStatusById DBookingStatus.FAILED bookingId
        throwE (IssuanceFailed $ "Operator declined to issue tickets: " <> T.pack (show err))
  when (null providerOrder.tickets) $ do
    lift $ QBooking.updateStatusById DBookingStatus.FAILED bookingId
    throwE (IssuanceFailed "Operator issued no tickets")

  tickets <- lift $ forM providerOrder.tickets (mkTicket booking now)
  lift (QTicket.createMany tickets)
  let issuedTotal = unitPrice * fromIntegral (length tickets) :: Double
      confirmedBooking =
        booking
          { DBooking.status = DBookingStatus.CONFIRMED,
            DBooking.bppOrderId = Just providerOrder.orderId,
            DBooking.totalPrice = booking.totalPrice{amount = realToFrac issuedTotal, amountInt = round issuedTotal}
          }
  lift (QBooking.updateByPrimaryKey confirmedBooking)
  bapId <-
    req.confirmReqContext.contextBapId
      & maybe (throwE (Unprocessable "BapId missing on confirm context")) pure
  lift (QRecon.create (mkRecon confirmedBooking merchant merchantOperatingCity becknConfig account bapId providerOrder.orderId paymentTxnId (length tickets) now))
  republish operator becknConfig order.orderBilling confirmedBooking tickets

mkRecon :: DBooking.FRFSTicketBooking -> DM.Merchant -> DMOC.MerchantOperatingCity -> DBC.BecknConfig -> OnInitACL.SettlementAccount -> Text -> Text -> Text -> Int -> UTCTime -> Recon.FRFSRecon
mkRecon booking merchant merchantOperatingCity becknConfig account bapId orderId paymentTxnId ticketQty now =
  Recon.FRFSRecon
    { Recon.overriddenAmount = Nothing,
      Recon.overrideAppliedEntityId = Nothing,
      Recon.overrideType = Nothing,
      Recon.id = Id (Common.sellerReconId orderId),
      Recon.frfsTicketBookingId = booking.id,
      Recon.networkOrderId = orderId,
      Recon.collectorSubscriberId = bapId,
      Recon.receiverSubscriberId = becknConfig.subscriberId,
      Recon.collectorIFSC = Nothing,
      Recon.beneficiaryIFSC = Just account.bankCode,
      Recon.beneficiaryBankAccount = Just account.bankAccountNumber,
      Recon.buyerFinderFee = booking.totalPrice{amount = 0, amountInt = 0},
      Recon.fare = booking.totalPrice,
      Recon.totalOrderValue = booking.totalPrice,
      Recon.settlementAmount = booking.totalPrice,
      Recon.differenceAmount = Just booking.totalPrice,
      Recon.reconStatus = Just Recon.PENDING,
      Recon.entityType = Just Recon.FRFS_TICKET_BOOKING,
      Recon.settlementDate = Nothing,
      Recon.settlementReferenceNumber = Nothing,
      Recon.sourceStationCode = Just booking.fromStationCode,
      Recon.destinationStationCode = Just booking.toStationCode,
      Recon.ticketNumber = Nothing,
      Recon.ticketQty = Just ticketQty,
      Recon.ticketStatus = Nothing,
      Recon.transactionRefNumber = Just paymentTxnId,
      Recon.transactionUUID = Just paymentTxnId,
      Recon.txnId = Just paymentTxnId,
      Recon.paymentGateway = Nothing,
      Recon.mobileNumber = Nothing,
      Recon.message = Nothing,
      Recon.date = show now,
      Recon.time = show now,
      Recon.providerId = booking.providerId,
      Recon.providerName = booking.providerName,
      Recon.merchantId = Just merchant.id,
      Recon.merchantOperatingCityId = Just merchantOperatingCity.id,
      Recon.createdAt = now,
      Recon.updatedAt = now
    }

republish :: Text -> DBC.BecknConfig -> Maybe Spec.Billing -> DBooking.FRFSTicketBooking -> [DTicket.FRFSTicket] -> ExceptT ConfirmFailure Flow ACL.ConfirmedOrder
republish operator becknConfig mbBilling booking tickets = do
  sellerConfig <- lift (SIBC.findIntegratedBPPConfigById booking.integratedBppConfigId)
  operatingHours <- lift (ExternalCallAPI.getOperatingHoursTags sellerConfig)
  operatingWindow <- lift (ExternalCallAPI.getOperatingWindow sellerConfig)
  stations <- lift (ExternalCallAPI.getStationList sellerConfig)
  let coordsOf = Common.stationCoords stations
  orderId <-
    booking.bppOrderId
      & maybe (throwE (Unprocessable $ "Confirmed booking " <> booking.id.getId <> " has no operator order id")) pure
  paymentTxnId <-
    booking.paymentTxnId
      & maybe (throwE (Unprocessable $ "Confirmed booking " <> booking.id.getId <> " has no payment transaction id")) pure
  paymentId <-
    booking.bppPaymentId
      & maybe (throwE (Unprocessable $ "Confirmed booking " <> booking.id.getId <> " has no payment id")) pure
  journeyType <-
    find (\journeyType -> journeyType.quoteType == booking._type) Common.sellerJourneyTypes
      & maybe (throwE (Unprocessable $ "Booking " <> booking.id.getId <> " has an unsellable journey type")) pure
  fromStopName <- booking.fromStationName & maybe (throwE (Unprocessable "Booking has no origin station name")) pure
  toStopName <- booking.toStationName & maybe (throwE (Unprocessable "Booking has no destination station name")) pure
  account <- either (throwE . Unprocessable) pure (Common.settlementAccount becknConfig (Common.formatPrice (realToFrac booking.totalPrice.amount)))
  let validity = Common.ticketValidity operator
      unitPrice = realToFrac booking.totalPrice.amount / fromIntegral (max 1 (length tickets)) :: Double
  let terms = Common.operatorTerms operator
  pure
    ACL.ConfirmedOrder
      { orderId,
        billing = mbBilling,
        courtJurisdiction = terms.courtJurisdiction,
        businessTermsUrl = terms.businessTermsUrl,
        cancellationTermsUrl = terms.cancellationTermsUrl,
        operatingWindow,
        fromStopLat = fst (coordsOf booking.fromStationCode),
        fromStopLon = snd (coordsOf booking.fromStationCode),
        toStopLat = fst (coordsOf booking.toStationCode),
        toStopLon = snd (coordsOf booking.toStationCode),
        maxPaidAreaMinutes = terms.maxPaidAreaMinutes,
        operatingHours,
        itemId = booking.bppItemId,
        journeyTypeCode = journeyType.code,
        journeyTypeName = journeyType.name,
        providerId = booking.providerId,
        providerName = booking.providerName,
        unitPrice = Common.formatPrice unitPrice,
        currency = show booking.totalPrice.currency,
        totalPrice = Common.formatPrice (unitPrice * fromIntegral (length tickets)),
        maxTicketsPerOrder = Common.maxTicketsPerOrder operator journeyType.code,
        fromStopCode = booking.fromStationCode,
        fromStopName,
        toStopCode = booking.toStationCode,
        toStopName,
        validityLabel = validity.label,
        validityDuration = validity.duration,
        paymentId,
        paymentTxnId,
        account,
        authorizationType = authorizationType,
        createdAt = booking.createdAt,
        tickets = zipWith (mkIssuedTicket booking) [0 ..] tickets
      }

mkIssuedTicket :: DBooking.FRFSTicketBooking -> Int -> DTicket.FRFSTicket -> ACL.IssuedTicket
mkIssuedTicket booking index ticket =
  ACL.IssuedTicket
    { fulfillmentId =
        if index == 0
          then fromMaybe ticket.ticketNumber journeyId
          else ticket.ticketNumber,
      ticketNumber = ticket.ticketNumber,
      qrToken = ticket.qrData,
      qrStatus = ACLUtils.wireTicketStatus ticket.status,
      validTill = ticket.validTill
    }
  where
    journeyId = do
      fromName <- booking.fromStationName
      toName <- booking.toStationName
      Common.journeyIdFromStationNames fromName toName

authorizationType :: Text
authorizationType = "QR"

ticketValiditySeconds :: Text -> NominalDiffTime
ticketValiditySeconds operator =
  case Common.ticketValidity operator & (.duration) of
    "PT120M" -> 120 * 60
    _ -> 24 * 60 * 60

qrTtl :: Seconds
qrTtl = 86400

mkBooking ::
  Id DBooking.FRFSTicketBooking ->
  Id DSearch.FRFSSearch ->
  DM.Merchant ->
  DMOC.MerchantOperatingCity ->
  DIBC.IntegratedBPPConfig ->
  DBC.BecknConfig ->
  QuoteCache.SellerQuote ->
  Common.SellerJourneyType ->
  OnInitACL.SettlementAccount ->
  Text ->
  Double ->
  Int ->
  UTCTime ->
  UTCTime ->
  DBooking.FRFSTicketBooking
mkBooking bookingId bookingSearchId merchant merchantOperatingCity integratedBPPConfig becknConfig quote journeyType account paymentTxnId unitPrice quantity validTill now =
  DBooking.FRFSTicketBooking
    { overriddenAmount = Nothing,
      overrideAppliedEntityId = Nothing,
      overrideType = Nothing,
      parentBookingId = Nothing,
      rescheduleCount = Nothing,
      _type = journeyType.quoteType,
      bookingAuthCode = Nothing,
      bppBankAccountNumber = Just account.bankAccountNumber,
      bppBankCode = Just account.bankCode,
      bppDelayedInterest = Nothing,
      bppItemId = quote.itemId,
      bppOrderId = Nothing,
      bppPaymentId = Just paymentId',
      bppSubscriberId = becknConfig.subscriberId,
      bppSubscriberUrl = showBaseUrl becknConfig.subscriberUrl,
      busLocationData = [],
      cancellationCharges = Nothing,
      cashbackPayoutOrderId = Nothing,
      cashbackStatus = Nothing,
      clientBundleVersion = Nothing,
      clientSdkVersion = Nothing,
      cloudType = Nothing,
      conductorId = Nothing,
      customerCancelled = False,
      discountedTickets = Nothing,
      driverId = Nothing,
      driverMobileNumber = Nothing,
      driverName = Nothing,
      eventDiscountAmount = Nothing,
      failureReason = Nothing,
      finalBoardedDepotNo = Nothing,
      finalBoardedScheduleNo = Nothing,
      finalBoardedVehicleNumber = Nothing,
      finalBoardedVehicleNumberSource = Nothing,
      finalBoardedVehicleServiceTierType = Nothing,
      finalBoardedWaybillId = Nothing,
      frfsTicketBookingPaymentIdForTicketGeneration = Nothing,
      fromStationAddress = Nothing,
      fromStationCode = quote.fromStopCode,
      fromStationName = Just quote.fromStopName,
      fromStationPoint = Nothing,
      fromStopIdx = Nothing,
      googleWalletJWTUrl = Nothing,
      holdId = Nothing,
      id = bookingId,
      integratedBppConfigId = integratedBPPConfig.id,
      isBookingCancellable = Nothing,
      isFareChanged = Nothing,
      isMockPayment = Nothing,
      isSingleMode = Nothing,
      isSpotBooking = Nothing,
      journeyOnInitDone = Nothing,
      merchantId = merchant.id,
      merchantOperatingCityId = merchantOperatingCity.id,
      multimodalSearchRequestId = Nothing,
      ondcOnInitReceived = Nothing,
      ondcOnInitReceivedAt = Nothing,
      osBuildVersion = Nothing,
      osType = Nothing,
      partnerOrgId = Nothing,
      partnerOrgTransactionId = Nothing,
      payerVpa = Nothing,
      paymentTxnId = Just paymentTxnId,
      providerDescription = Nothing,
      providerId = quote.providerId,
      providerName = quote.providerName,
      quoteId = Id bookingId.getId,
      recentLocationId = Nothing,
      refundAmount = Nothing,
      riderId = Common.sellerRiderId,
      routeCode = Nothing,
      routeName = Nothing,
      routeStationsJson = Nothing,
      searchId = bookingSearchId,
      seatSelectionType = Nothing,
      serviceTierType = Nothing,
      startTime = Nothing,
      stationsJson = "[]",
      status = DBookingStatus.CONFIRMING,
      toStationAddress = Nothing,
      toStationCode = quote.toStopCode,
      toStationName = Just quote.toStopName,
      toStationPoint = Nothing,
      toStopIdx = Nothing,
      totalPrice = Price {amountInt = round total, amount = realToFrac total, currency = INR},
      tripId = Nothing,
      validTill,
      vehicleNumber = Nothing,
      vehicleType = SpecEnums.METRO,
      waybillNo = Nothing,
      createdAt = now,
      updatedAt = now
    }
  where
    total = unitPrice * fromIntegral quantity :: Double
    paymentId' = bookingId.getId

mkQuoteCategory :: Id DBooking.FRFSTicketBooking -> DM.Merchant -> DMOC.MerchantOperatingCity -> QuoteCache.SellerQuote -> Double -> Int -> UTCTime -> DQuoteCategory.FRFSQuoteCategory
mkQuoteCategory bookingId merchant merchantOperatingCity quote unitPrice quantity now =
  DQuoteCategory.FRFSQuoteCategory
    { bppItemId = quote.itemId,
      category = DQuoteCategoryType.ADULT,
      categoryMeta = Nothing,
      finalPrice = Just price,
      holdId = Nothing,
      id = Id bookingId.getId,
      merchantId = merchant.id,
      merchantOperatingCityId = merchantOperatingCity.id,
      offeredPrice = price,
      price = price,
      quoteId = Id bookingId.getId,
      seatIds = Nothing,
      seatLabels = Nothing,
      selectedQuantity = quantity,
      createdAt = now,
      updatedAt = now
    }
  where
    price = Price {amountInt = round unitPrice, amount = realToFrac unitPrice, currency = INR}

mkTicket :: DBooking.FRFSTicketBooking -> UTCTime -> ExternalTypes.ProviderTicket -> Flow DTicket.FRFSTicket
mkTicket booking now providerTicket = do
  ticketId <- generateGUID
  pure
    DTicket.FRFSTicket
      { cloudType = Nothing,
        commencingHours = providerTicket.commencingHours,
        description = providerTicket.description,
        frfsTicketBookingId = booking.id,
        id = ticketId,
        isReturnTicket = Just (booking._type == DQuote.ReturnJourney),
        isTicketFree = Just False,
        merchantId = booking.merchantId,
        merchantOperatingCityId = booking.merchantOperatingCityId,
        partnerOrgId = Nothing,
        partnerOrgTransactionId = Nothing,
        qrData = providerTicket.qrData,
        qrRefreshAt = providerTicket.qrRefreshAt,
        riderId = booking.riderId,
        scannedByVehicleNumber = Nothing,
        status = DTicketStatus.ACTIVE,
        ticketNumber = providerTicket.ticketNumber,
        validTill = providerTicket.qrValidity,
        createdAt = now,
        updatedAt = now
      }
