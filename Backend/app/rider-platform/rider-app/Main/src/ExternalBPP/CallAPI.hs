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
import qualified Domain.Action.Beckn.FRFS.OnCancel as DOnCancel
import qualified Domain.Action.Beckn.FRFS.OnInit as DOnInit
import qualified Domain.Action.Beckn.FRFS.OnSearch as DOnSearch
import qualified Domain.Action.Beckn.FRFS.OnStatus as DOnStatus
import Domain.Types.BecknConfig
import qualified Domain.Types.FRFSQuote as DQuote
import Domain.Types.FRFSRouteDetails
import Domain.Types.FRFSSearch as DSearch
import qualified Domain.Types.FRFSTicket as DFRFSTicket
import qualified Domain.Types.FRFSTicketBooking as DBooking
import Domain.Types.IntegratedBPPConfig
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity
import Environment
import EulerHS.Prelude ((+||), (||+))
import qualified ExternalBPP.Flow as Flow
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Payment.Storage.Beam.BeamFlow
import qualified SharedLogic.CallFRFSBPP as CallFRFSBPP
import qualified Storage.CachedQueries.FRFSConfig as CQFRFSConfig
import Storage.CachedQueries.IntegratedBPPConfig as QIBC
import qualified Storage.CachedQueries.Station as QStation
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

discoverySearch :: FRFSSearchFlow m r => Merchant -> BecknConfig -> API.Types.UI.FRFSTicketService.FRFSDiscoverySearchAPIReq -> m ()
discoverySearch merchant bapConfig req = do
  transactionId <- generateGUID
  bknSearchReq <- ACL.buildSearchReq transactionId req.vehicleType bapConfig Nothing Nothing req.city
  logDebug $ "FRFS Discovery SearchReq " <> encodeToText bknSearchReq
  void $ CallFRFSBPP.search bapConfig.gatewayUrl bknSearchReq merchant.id

search :: (FRFSSearchFlow m r, HasShortDurationRetryCfg r c) => Merchant -> MerchantOperatingCity -> BecknConfig -> DSearch.FRFSSearch -> [FRFSRouteDetails] -> IntegratedBPPConfig -> m ()
search merchant merchantOperatingCity bapConfig searchReq routeDetails integratedBPPConfig = do
  case integratedBPPConfig.providerConfig of
    ONDC _ -> do
      fork ("FRFS ONDC SearchReq for " <> show bapConfig.vehicleCategory) $ do
        fromStation <- QStation.findById searchReq.fromStationId >>= fromMaybeM (StationNotFound searchReq.fromStationId.getId)
        toStation <- QStation.findById searchReq.toStationId >>= fromMaybeM (StationNotFound searchReq.toStationId.getId)
        bknSearchReq <- ACL.buildSearchReq searchReq.id.getId searchReq.vehicleType bapConfig (Just fromStation) (Just toStation) merchantOperatingCity.city
        logDebug $ "FRFS SearchReq " <> encodeToText bknSearchReq
        Metrics.startMetrics Metrics.SEARCH_FRFS merchant.name searchReq.id.getId merchantOperatingCity.id.getId
        void $ CallFRFSBPP.search bapConfig.gatewayUrl bknSearchReq merchant.id
    _ -> do
      fork "FRFS External SearchReq" $ do
        onSearchReq <- Flow.search merchant merchantOperatingCity integratedBPPConfig bapConfig searchReq routeDetails
        processOnSearch onSearchReq
  where
    processOnSearch :: (FRFSSearchFlow m r, HasShortDurationRetryCfg r c) => DOnSearch.DOnSearch -> m ()
    processOnSearch onSearchReq = do
      validatedDOnSearch <- DOnSearch.validateRequest onSearchReq
      DOnSearch.onSearch onSearchReq validatedDOnSearch

select ::
  FRFSSelectFlow m r c =>
  (DOnSelect -> m ()) ->
  Merchant ->
  MerchantOperatingCity ->
  BecknConfig ->
  DQuote.FRFSQuote ->
  DIBC.PlatformType ->
  Spec.VehicleCategory ->
  m ()
select processOnSelectHandler merchant merchantOperatingCity bapConfig quote platformType vehicleType = do
  integratedBPPConfig <- QIBC.findByDomainAndCityAndVehicleCategory (show Spec.FRFS) merchantOperatingCity.id (frfsVehicleCategoryToBecknVehicleCategory vehicleType) platformType >>= fromMaybeM (IntegratedBPPConfigNotFound $ "MerchantOperatingCityId:" +|| merchantOperatingCity.id.getId ||+ "Domain:" +|| Spec.FRFS ||+ "Vehicle:" +|| frfsVehicleCategoryToBecknVehicleCategory vehicleType ||+ "Platform Type:" +|| platformType ||+ "")
  case integratedBPPConfig.providerConfig of
    ONDC _ -> do
      fork ("FRFS ONDC SelectReq for " <> show bapConfig.vehicleCategory) $ do
        providerUrl <- quote.bppSubscriberUrl & parseBaseUrl
        bknSelectReq <- ACL.buildSelectReq quote bapConfig Utils.BppData {bppId = quote.bppSubscriberId, bppUri = quote.bppSubscriberUrl} merchantOperatingCity.city
        logDebug $ "FRFS SelectReq " <> encodeToText bknSelectReq
        Metrics.startMetrics Metrics.SELECT_FRFS merchant.name quote.searchId.getId merchantOperatingCity.id.getId
        void $ CallFRFSBPP.select providerUrl bknSelectReq merchant.id
    _ -> do
      onSelectReq <- Flow.select merchant merchantOperatingCity integratedBPPConfig bapConfig quote
      processOnSelectHandler onSelectReq

init :: FRFSConfirmFlow m r => Merchant -> MerchantOperatingCity -> BecknConfig -> (Maybe Text, Maybe Text) -> DBooking.FRFSTicketBooking -> DIBC.PlatformType -> m ()
init merchant merchantOperatingCity bapConfig (mRiderName, mRiderNumber) booking platformType = do
  integratedBPPConfig <- QIBC.findByDomainAndCityAndVehicleCategory (show Spec.FRFS) merchantOperatingCity.id (frfsVehicleCategoryToBecknVehicleCategory booking.vehicleType) platformType >>= fromMaybeM (IntegratedBPPConfigNotFound $ "MerchantOperatingCityId:" +|| merchantOperatingCity.id.getId ||+ "Domain:" +|| Spec.FRFS ||+ "Vehicle:" +|| frfsVehicleCategoryToBecknVehicleCategory booking.vehicleType ||+ "Platform Type:" +|| platformType ||+ "")
  case integratedBPPConfig.providerConfig of
    ONDC _ -> do
      providerUrl <- booking.bppSubscriberUrl & parseBaseUrl & fromMaybeM (InvalidRequest "Invalid provider url")
      bknInitReq <- ACL.buildInitReq (mRiderName, mRiderNumber) booking bapConfig Utils.BppData {bppId = booking.bppSubscriberId, bppUri = booking.bppSubscriberUrl} merchantOperatingCity.city
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

cancel :: Merchant -> MerchantOperatingCity -> BecknConfig -> Spec.CancellationType -> DBooking.FRFSTicketBooking -> DIBC.PlatformType -> Flow ()
cancel merchant merchantOperatingCity bapConfig cancellationType booking platformType = do
  integratedBPPConfig <- QIBC.findByDomainAndCityAndVehicleCategory (show Spec.FRFS) merchantOperatingCity.id (frfsVehicleCategoryToBecknVehicleCategory booking.vehicleType) platformType >>= fromMaybeM (IntegratedBPPConfigNotFound $ "MerchantOperatingCityId:" +|| merchantOperatingCity.id.getId ||+ "Domain:" +|| Spec.FRFS ||+ "Vehicle:" +|| frfsVehicleCategoryToBecknVehicleCategory booking.vehicleType ||+ "Platform Type:" +|| platformType ||+ "")
  case integratedBPPConfig.providerConfig of
    ONDC _ -> do
      fork "FRFS ONDC Cancel Req" $ do
        frfsConfig <-
          CQFRFSConfig.findByMerchantOperatingCityIdInRideFlow merchantOperatingCity.id []
            >>= fromMaybeM (InternalError $ "FRFS config not found for merchant operating city Id " <> merchantOperatingCity.id.getId)
        providerUrl <- booking.bppSubscriberUrl & parseBaseUrl & fromMaybeM (InvalidRequest "Invalid provider url")
        ttl <- bapConfig.cancelTTLSec & fromMaybeM (InternalError "Invalid ttl")
        when (cancellationType == Spec.CONFIRM_CANCEL) $ Redis.setExp (DOnCancel.makecancelledTtlKey booking.id) True ttl
        bknCancelReq <- ACL.buildCancelReq booking bapConfig Utils.BppData {bppId = booking.bppSubscriberId, bppUri = booking.bppSubscriberUrl} frfsConfig.cancellationReasonId cancellationType merchantOperatingCity.city
        logDebug $ "FRFS CancelReq " <> encodeToText bknCancelReq
        void $ CallFRFSBPP.cancel providerUrl bknCancelReq merchant.id
    _ -> return ()

confirm :: (DOrder -> Flow ()) -> Merchant -> MerchantOperatingCity -> BecknConfig -> (Maybe Text, Maybe Text) -> DBooking.FRFSTicketBooking -> DIBC.PlatformType -> Flow ()
confirm onConfirmHandler merchant merchantOperatingCity bapConfig (mRiderName, mRiderNumber) booking platformType = do
  integratedBPPConfig <- QIBC.findByDomainAndCityAndVehicleCategory (show Spec.FRFS) merchantOperatingCity.id (frfsVehicleCategoryToBecknVehicleCategory booking.vehicleType) platformType >>= fromMaybeM (IntegratedBPPConfigNotFound $ "MerchantOperatingCityId:" +|| merchantOperatingCity.id.getId ||+ "Domain:" +|| Spec.FRFS ||+ "Vehicle:" +|| frfsVehicleCategoryToBecknVehicleCategory booking.vehicleType ||+ "Platform Type:" +|| platformType ||+ "")
  case integratedBPPConfig.providerConfig of
    ONDC _ -> do
      fork "FRFS ONDC Confirm Req" $ do
        providerUrl <- booking.bppSubscriberUrl & parseBaseUrl & fromMaybeM (InvalidRequest "Invalid provider url")
        bknConfirmReq <- ACL.buildConfirmReq (mRiderName, mRiderNumber) booking bapConfig booking.searchId.getId Utils.BppData {bppId = booking.bppSubscriberId, bppUri = booking.bppSubscriberUrl} merchantOperatingCity.city
        logDebug $ "FRFS ConfirmReq " <> encodeToText bknConfirmReq
        Metrics.startMetrics Metrics.CONFIRM_FRFS merchant.name booking.searchId.getId merchantOperatingCity.id.getId
        void $ CallFRFSBPP.confirm providerUrl bknConfirmReq merchant.id
    _ -> do
      fork "FRFS External Confirm Req" $ do
        frfsConfig <-
          CQFRFSConfig.findByMerchantOperatingCityIdInRideFlow merchantOperatingCity.id []
            >>= fromMaybeM (InternalError $ "FRFS config not found for merchant operating city Id " <> merchantOperatingCity.id.getId)
        onConfirmReq <- Flow.confirm merchant merchantOperatingCity frfsConfig integratedBPPConfig bapConfig (mRiderName, mRiderNumber) booking
        onConfirmHandler onConfirmReq

status :: Id Merchant -> MerchantOperatingCity -> BecknConfig -> DBooking.FRFSTicketBooking -> DIBC.PlatformType -> Flow ()
status merchantId merchantOperatingCity bapConfig booking platformType = do
  integratedBPPConfig <- QIBC.findByDomainAndCityAndVehicleCategory (show Spec.FRFS) merchantOperatingCity.id (frfsVehicleCategoryToBecknVehicleCategory booking.vehicleType) platformType >>= fromMaybeM (IntegratedBPPConfigNotFound $ "MerchantOperatingCityId:" +|| merchantOperatingCity.id.getId ||+ "Domain:" +|| Spec.FRFS ||+ "Vehicle:" +|| frfsVehicleCategoryToBecknVehicleCategory booking.vehicleType ||+ "Platform Type:" +|| platformType ||+ "")
  case integratedBPPConfig.providerConfig of
    ONDC _ -> do
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
  integratedBPPConfig <- QIBC.findByDomainAndCityAndVehicleCategory (show Spec.FRFS) merchantOperatingCity.id (frfsVehicleCategoryToBecknVehicleCategory vehicleCategory) platformType >>= fromMaybeM (IntegratedBPPConfigNotFound $ "MerchantOperatingCityId:" +|| merchantOperatingCity.id.getId ||+ "Domain:" +|| Spec.FRFS ||+ "Vehicle:" +|| frfsVehicleCategoryToBecknVehicleCategory vehicleCategory ||+ "Platform Type:" +|| platformType ||+ "")
  onStatusReq <- Flow.verifyTicket merchantId merchantOperatingCity integratedBPPConfig bapConfig encryptedQrData
  processOnStatus onStatusReq >>= \case
    DOnStatus.TicketVerificationSync ticket -> return ticket
    _ -> throwError $ InvalidRequest "Unsupported OnStatus Response."
  where
    processOnStatus onStatusReq = do
      let verificationOnStatusReq = DOnStatus.TicketVerification onStatusReq
      (merchant', booking') <- DOnStatus.validateRequest verificationOnStatusReq
      void $ status merchantId merchantOperatingCity bapConfig booking' platformType -- Doing one status call to sync the Ticket Status & Expiry Before Verifying
      DOnStatus.onStatus merchant' booking' verificationOnStatusReq
