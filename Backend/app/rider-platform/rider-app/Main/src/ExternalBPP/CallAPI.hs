module ExternalBPP.CallAPI where

import qualified API.Types.UI.FRFSTicketService
import qualified Beckn.ACL.FRFS.Cancel as ACL
import qualified Beckn.ACL.FRFS.Confirm as ACL
import qualified Beckn.ACL.FRFS.Init as ACL
import qualified Beckn.ACL.FRFS.Search as ACL
import qualified Beckn.ACL.FRFS.Select as ACL
import qualified Beckn.ACL.FRFS.Utils as Utils
import qualified BecknV2.FRFS.Enums as Spec
import BecknV2.FRFS.Utils
import Domain.Action.Beckn.FRFS.Common hiding (status)
import qualified Domain.Action.Beckn.FRFS.OnInit as DOnInit
import qualified Domain.Action.Beckn.FRFS.OnSearch as DOnSearch
import qualified Domain.Action.Beckn.FRFS.OnStatus as DOnStatus
import Domain.Types.BecknConfig
import qualified Domain.Types.FRFSQuote as DQuote
import Domain.Types.FRFSRouteDetails
import Domain.Types.FRFSSearch as DSearch
import qualified Domain.Types.FRFSTicket as DFRFSTicket
import qualified Domain.Types.FRFSTicketBooking as DBooking
import qualified Domain.Types.FRFSTicketBookingPayment as DFRFSTicketBookingPayment
import qualified Domain.Types.FRFSTicketBookingStatus as DBooking
import Domain.Types.IntegratedBPPConfig
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Station as DStation
import Environment
import qualified ExternalBPP.Flow as Flow
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Payment.Storage.Beam.BeamFlow
import qualified SharedLogic.CallFRFSBPP as CallFRFSBPP
import SharedLogic.FRFSUtils as FRFSUtils
import qualified SharedLogic.IntegratedBPPConfig as SIBC
import qualified Storage.CachedQueries.FRFSConfig as CQFRFSConfig
import qualified Storage.CachedQueries.Merchant.RiderConfig as QRC
import qualified Storage.CachedQueries.OTPRest.OTPRest as OTPRest
import qualified Storage.Queries.FRFSQuote as QFRFSQuote
import qualified Storage.Queries.FRFSQuoteCategory as QFRFSQuoteCategory
import qualified Storage.Queries.FRFSTicketBooking as QFRFSTicketBooking
import qualified Storage.Queries.FRFSTicketBookingPayment as QFRFSTicketBookingPayment
import Tools.Error
import qualified Tools.Metrics as Metrics

type FRFSSearchFlow m r =
  ( CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    Metrics.HasBAPMetrics m r,
    CallFRFSBPP.BecknAPICallFlow m r,
    EncFlow m r
  )

type FRFSConfirmFlow m r =
  ( MonadFlow m,
    BeamFlow m r,
    EsqDBReplicaFlow m r,
    Metrics.HasBAPMetrics m r,
    CallFRFSBPP.BecknAPICallFlow m r,
    EncFlow m r,
    ServiceFlow m r,
    HasField "isMetroTestTransaction" r Bool
  )

type FRFSSelectFlow m r c =
  ( MonadFlow m,
    BeamFlow m r,
    EsqDBReplicaFlow m r,
    HasShortDurationRetryCfg r c,
    Metrics.HasBAPMetrics m r,
    CallFRFSBPP.BecknAPICallFlow m r,
    EncFlow m r,
    ServiceFlow m r,
    HasField "isMetroTestTransaction" r Bool
  )

discoverySearch :: FRFSSearchFlow m r => Merchant -> BecknConfig -> IntegratedBPPConfig -> API.Types.UI.FRFSTicketService.FRFSDiscoverySearchAPIReq -> m ()
discoverySearch merchant bapConfig _integratedBPPConfigId req = do
  transactionId <- generateGUID
  bknSearchReq <- ACL.buildSearchReq transactionId req.vehicleType bapConfig Nothing Nothing req.city
  logDebug $ "FRFS Discovery SearchReq " <> encodeToText bknSearchReq
  void $ CallFRFSBPP.search bapConfig.gatewayUrl bknSearchReq merchant.id

search :: (FRFSSearchFlow m r, HasShortDurationRetryCfg r c) => Merchant -> MerchantOperatingCity -> BecknConfig -> DSearch.FRFSSearch -> Maybe HighPrecMoney -> [FRFSRouteDetails] -> IntegratedBPPConfig -> m ()
search merchant merchantOperatingCity bapConfig searchReq mbFare routeDetails integratedBPPConfig = do
  case integratedBPPConfig.providerConfig of
    ONDC ONDCBecknConfig {networkHostUrl, networkId, fareCachingAllowed} -> do
      fork ("FRFS ONDC SearchReq for " <> show bapConfig.vehicleCategory) $ do
        case (fareCachingAllowed, networkHostUrl, networkId, mbFare) of
          (Just True, Just _, Just _, Just _) ->
            try @_ @SomeException (Flow.search merchant merchantOperatingCity integratedBPPConfig bapConfig networkHostUrl networkId searchReq routeDetails)
              >>= \case
                Left err -> do
                  logError $ "Error in calling ONDC Search: " <> show err
                  callOndcSearch networkHostUrl
                Right onSearchReq ->
                  if null onSearchReq.quotes
                    then callOndcSearch networkHostUrl
                    else processOnSearch onSearchReq
          _ -> callOndcSearch networkHostUrl
    _ -> do
      fork "FRFS Direct SearchReq" $ do
        onSearchReq <- Flow.search merchant merchantOperatingCity integratedBPPConfig bapConfig Nothing Nothing searchReq routeDetails
        processOnSearch onSearchReq
  where
    processOnSearch :: (FRFSSearchFlow m r, HasShortDurationRetryCfg r c) => DOnSearch.DOnSearch -> m ()
    processOnSearch onSearchReq = do
      validatedDOnSearch <- DOnSearch.validateRequest onSearchReq
      DOnSearch.onSearch onSearchReq validatedDOnSearch

    callOndcSearch :: Maybe BaseUrl -> (FRFSSearchFlow m r, HasShortDurationRetryCfg r c) => m ()
    callOndcSearch networkHostUrl = do
      let providerUrl = fromMaybe bapConfig.gatewayUrl networkHostUrl
      fromStation <- OTPRest.getStationByGtfsIdAndStopCode searchReq.fromStationCode integratedBPPConfig >>= fromMaybeM (StationNotFound searchReq.fromStationCode)
      toStation <- OTPRest.getStationByGtfsIdAndStopCode searchReq.toStationCode integratedBPPConfig >>= fromMaybeM (StationNotFound searchReq.toStationCode)
      routeStopMappingFromStation <- OTPRest.getRouteStopMappingByStopCode searchReq.fromStationCode integratedBPPConfig
      routeStopMappingToStation <- OTPRest.getRouteStopMappingByStopCode searchReq.toStationCode integratedBPPConfig
      let fromStationProviderCode = fromMaybe searchReq.fromStationCode (listToMaybe routeStopMappingFromStation <&> (.providerCode))
          toStationProviderCode = fromMaybe searchReq.toStationCode (listToMaybe routeStopMappingToStation <&> (.providerCode))
      bknSearchReq <- ACL.buildSearchReq searchReq.id.getId searchReq.vehicleType bapConfig (Just $ fromStation {DStation.code = fromStationProviderCode}) (Just $ toStation {DStation.code = toStationProviderCode}) merchantOperatingCity.city
      logDebug $ "FRFS SearchReq " <> encodeToText bknSearchReq
      Metrics.startMetrics Metrics.SEARCH_FRFS merchant.name searchReq.id.getId merchantOperatingCity.id.getId
      void $ CallFRFSBPP.search providerUrl bknSearchReq merchant.id

select ::
  FRFSSelectFlow m r c =>
  (DOnSelect -> m ()) ->
  Merchant ->
  MerchantOperatingCity ->
  BecknConfig ->
  DQuote.FRFSQuote ->
  Maybe Int ->
  Maybe Int ->
  Maybe [API.Types.UI.FRFSTicketService.FRFSCategorySelectionReq] ->
  m ()
select processOnSelectHandler merchant merchantOperatingCity bapConfig quote ticketQuantity childTicketQuantity categorySelectionReq = do
  integratedBPPConfig <- SIBC.findIntegratedBPPConfigFromEntity quote
  case integratedBPPConfig.providerConfig of
    ONDC _ -> do
      fork ("FRFS ONDC SelectReq for " <> show bapConfig.vehicleCategory) $ do
        void $ QFRFSQuote.updateTicketAndChildTicketQuantityById quote.id ticketQuantity childTicketQuantity
        quoteCategories <- QFRFSQuoteCategory.findAllByQuoteId quote.id
        updatedQuoteCategories <- FRFSUtils.updateQuoteCategoriesWithSelections (fromMaybe [] categorySelectionReq) quoteCategories
        let updatedQuantity = fromMaybe quote.quantity ticketQuantity
        let updatedQuote = quote {DQuote.quantity = updatedQuantity, DQuote.childTicketQuantity = childTicketQuantity}
        let categories =
              mapMaybe
                ( \category -> do
                    selectedQuantity <- category.selectedQuantity
                    return $ DCategorySelect {bppItemId = category.bppItemId, quantity = selectedQuantity}
                )
                updatedQuoteCategories

        providerUrl <- quote.bppSubscriberUrl & parseBaseUrl
        bknSelectReq <- ACL.buildSelectReq updatedQuote bapConfig Utils.BppData {bppId = quote.bppSubscriberId, bppUri = quote.bppSubscriberUrl} merchantOperatingCity.city categories
        logDebug $ "FRFS SelectReq " <> encodeToText bknSelectReq
        Metrics.startMetrics Metrics.SELECT_FRFS merchant.name quote.searchId.getId merchantOperatingCity.id.getId
        void $ CallFRFSBPP.select providerUrl bknSelectReq merchant.id
    _ -> do
      onSelectReq <- Flow.select merchant merchantOperatingCity integratedBPPConfig bapConfig quote ticketQuantity childTicketQuantity categorySelectionReq
      processOnSelectHandler onSelectReq

init :: FRFSConfirmFlow m r => Merchant -> MerchantOperatingCity -> BecknConfig -> (Maybe Text, Maybe Text) -> DBooking.FRFSTicketBooking -> [DCategorySelect] -> m ()
init merchant merchantOperatingCity bapConfig (mRiderName, mRiderNumber) booking categories = do
  integratedBPPConfig <- SIBC.findIntegratedBPPConfigFromEntity booking
  case integratedBPPConfig.providerConfig of
    ONDC _ -> do
      providerUrl <- booking.bppSubscriberUrl & parseBaseUrl & fromMaybeM (InvalidRequest "Invalid provider url")
      bknInitReq <- ACL.buildInitReq (mRiderName, mRiderNumber) booking bapConfig Utils.BppData {bppId = booking.bppSubscriberId, bppUri = booking.bppSubscriberUrl} merchantOperatingCity.city categories
      logDebug $ "FRFS InitReq " <> encodeToText bknInitReq
      Metrics.startMetrics Metrics.INIT_FRFS merchant.name booking.searchId.getId merchantOperatingCity.id.getId
      void $ CallFRFSBPP.init providerUrl bknInitReq merchant.id
    _ -> do
      onInitReq <- Flow.init merchant merchantOperatingCity integratedBPPConfig bapConfig (mRiderName, mRiderNumber) booking
      processOnInit onInitReq
  where
    processOnInit :: FRFSConfirmFlow m r => DOnInit.DOnInit -> m ()
    processOnInit onInitReq = do
      (merchant', booking') <- DOnInit.validateRequest onInitReq
      DOnInit.onInit onInitReq merchant' booking'

cancel :: Merchant -> MerchantOperatingCity -> BecknConfig -> Spec.CancellationType -> DBooking.FRFSTicketBooking -> Flow ()
cancel merchant merchantOperatingCity bapConfig cancellationType booking = do
  integratedBPPConfig <- SIBC.findIntegratedBPPConfigFromEntity booking
  frfsConfig <-
    CQFRFSConfig.findByMerchantOperatingCityIdInRideFlow merchantOperatingCity.id []
      >>= fromMaybeM (InternalError $ "FRFS config not found for merchant operating city Id " <> merchantOperatingCity.id.getId)
  unless (frfsConfig.isCancellationAllowed) $ throwError CancellationNotSupported
  when (cancellationType == Spec.SOFT_CANCEL) $
    unless (booking.status == DBooking.CONFIRMED) $ throwError (InvalidRequest $ "Cancellation during incorrect status: " <> show booking.status)
  case integratedBPPConfig.providerConfig of
    ONDC _ -> do
      fork "FRFS ONDC Cancel Req" $ do
        providerUrl <- booking.bppSubscriberUrl & parseBaseUrl & fromMaybeM (InvalidRequest "Invalid provider url")
        ttl <- bapConfig.cancelTTLSec & fromMaybeM (InternalError "Invalid ttl")
        messageId <- generateGUID
        when (cancellationType == Spec.CONFIRM_CANCEL) $ Redis.setExp (FRFSUtils.makecancelledTtlKey booking.id) messageId ttl
        bknCancelReq <- ACL.buildCancelReq messageId booking bapConfig Utils.BppData {bppId = booking.bppSubscriberId, bppUri = booking.bppSubscriberUrl} frfsConfig.cancellationReasonId cancellationType merchantOperatingCity.city
        logDebug $ "FRFS CancelReq " <> encodeToText bknCancelReq
        void $ CallFRFSBPP.cancel providerUrl bknCancelReq merchant.id
    _ -> return ()

confirm :: (DOrder -> Flow ()) -> Merchant -> MerchantOperatingCity -> BecknConfig -> (Maybe Text, Maybe Text) -> DBooking.FRFSTicketBooking -> Flow ()
confirm onConfirmHandler merchant merchantOperatingCity bapConfig (mRiderName, mRiderNumber) booking = do
  integratedBPPConfig <- SIBC.findIntegratedBPPConfigFromEntity booking
  case integratedBPPConfig.providerConfig of
    ONDC _ -> do
      fork "FRFS ONDC Confirm Req" $ do
        providerUrl <- booking.bppSubscriberUrl & parseBaseUrl & fromMaybeM (InvalidRequest "Invalid provider url")
        categories <- QFRFSQuoteCategory.findAllByQuoteId booking.quoteId
        let filteredDCategories :: [DCategorySelect] =
              mapMaybe
                ( \category -> do
                    selectedQuantity <- category.selectedQuantity
                    return $ DCategorySelect {bppItemId = category.bppItemId, quantity = selectedQuantity}
                )
                categories
        bknConfirmReq <- ACL.buildConfirmReq (mRiderName, mRiderNumber) booking bapConfig booking.searchId.getId Utils.BppData {bppId = booking.bppSubscriberId, bppUri = booking.bppSubscriberUrl} merchantOperatingCity.city filteredDCategories
        logDebug $ "FRFS ConfirmReq " <> encodeToText bknConfirmReq
        Metrics.startMetrics Metrics.CONFIRM_FRFS merchant.name booking.searchId.getId merchantOperatingCity.id.getId
        void $ CallFRFSBPP.confirm providerUrl bknConfirmReq merchant.id
    _ -> do
      fork "FRFS External Confirm Req" $ do
        result <- try @_ @SomeException $ do
          frfsConfig <-
            CQFRFSConfig.findByMerchantOperatingCityIdInRideFlow merchantOperatingCity.id []
              >>= fromMaybeM (InternalError $ "FRFS config not found for merchant operating city Id " <> merchantOperatingCity.id.getId)
          onConfirmReq <- Flow.confirm merchant merchantOperatingCity frfsConfig integratedBPPConfig bapConfig (mRiderName, mRiderNumber) booking
          onConfirmHandler onConfirmReq
        case result of
          Left err -> do
            void $ QFRFSTicketBooking.updateStatusById DBooking.FAILED booking.id
            void $ QFRFSTicketBookingPayment.updateStatusByTicketBookingId DFRFSTicketBookingPayment.REFUND_PENDING booking.id
            riderConfig <- QRC.findByMerchantOperatingCityId merchantOperatingCity.id Nothing >>= fromMaybeM (RiderConfigDoesNotExist merchantOperatingCity.id.getId)
            when riderConfig.enableAutoJourneyRefund $
              FRFSUtils.markAllRefundBookings booking booking.riderId
            throwM err
          Right _ -> return ()

status :: Id Merchant -> MerchantOperatingCity -> BecknConfig -> DBooking.FRFSTicketBooking -> Flow ()
status merchantId merchantOperatingCity bapConfig booking = do
  integratedBPPConfig <- SIBC.findIntegratedBPPConfigFromEntity booking
  case integratedBPPConfig.providerConfig of
    ONDC _ -> do
      Redis.setExp (Utils.mkCheckInprogressKey booking.searchId.getId) False 86400 -- 24hours
      void $ CallFRFSBPP.callBPPStatus booking bapConfig merchantOperatingCity.city merchantId
    _ -> do
      onStatusReq <- Flow.status merchantId merchantOperatingCity integratedBPPConfig bapConfig booking
      void $ processOnStatus onStatusReq
  where
    processOnStatus onStatusReq = do
      let bookingOnStatusReq = DOnStatus.Booking onStatusReq
      (merchant', booking') <- DOnStatus.validateRequest bookingOnStatusReq
      DOnStatus.onStatus merchant' booking' bookingOnStatusReq

verifyTicket :: Id Merchant -> MerchantOperatingCity -> BecknConfig -> Spec.VehicleCategory -> Text -> DIBC.PlatformType -> Flow DFRFSTicket.FRFSTicket
verifyTicket merchantId merchantOperatingCity bapConfig vehicleCategory encryptedQrData platformType = do
  -- TODO: Add support for multiple integratedBPPConfigs, when we have multiple integratedBPPConfigs integrating verifyTicket API
  integratedBPPConfig <- SIBC.findIntegratedBPPConfig Nothing merchantOperatingCity.id (frfsVehicleCategoryToBecknVehicleCategory vehicleCategory) platformType
  onStatusReq <- Flow.verifyTicket merchantId merchantOperatingCity integratedBPPConfig bapConfig encryptedQrData
  processOnStatus onStatusReq >>= \case
    DOnStatus.TicketVerificationSync ticket -> return ticket
    _ -> throwError $ InvalidRequest "Unsupported OnStatus Response."
  where
    processOnStatus onStatusReq = do
      let verificationOnStatusReq = DOnStatus.TicketVerification onStatusReq
      (merchant', booking') <- DOnStatus.validateRequest verificationOnStatusReq
      void $ status merchantId merchantOperatingCity bapConfig booking' -- Doing one status call to sync the Ticket Status & Expiry Before Verifying
      DOnStatus.onStatus merchant' booking' verificationOnStatusReq
