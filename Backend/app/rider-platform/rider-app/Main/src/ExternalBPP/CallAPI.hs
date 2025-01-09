module ExternalBPP.CallAPI where

import qualified Beckn.ACL.FRFS.Cancel as ACL
import qualified Beckn.ACL.FRFS.Confirm as ACL
import qualified Beckn.ACL.FRFS.Init as ACL
import qualified Beckn.ACL.FRFS.Search as ACL
import qualified Beckn.ACL.FRFS.Utils as Utils
import qualified BecknV2.FRFS.Enums as Spec
import BecknV2.FRFS.Utils
import BecknV2.OnDemand.Enums
import Domain.Action.Beckn.FRFS.Common
import qualified Domain.Action.Beckn.FRFS.OnCancel as DOnCancel
import qualified Domain.Action.Beckn.FRFS.OnInit as DOnInit
import qualified Domain.Action.Beckn.FRFS.OnSearch as DOnSearch
import qualified Domain.Action.Beckn.FRFS.OnStatus as DOnStatus
import Domain.Types.BecknConfig
import Domain.Types.FRFSSearch as DSearch
import qualified Domain.Types.FRFSTicketBooking as DBooking
import Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity
import Environment
import qualified ExternalBPP.Bus.Flow as BusFlow
import qualified ExternalBPP.Metro.Flow as MetroFlow
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

search :: FRFSSearchFlow m r => Merchant -> MerchantOperatingCity -> BecknConfig -> DSearch.FRFSSearch -> m ()
search merchant merchantOperatingCity bapConfig searchReq = do
  integratedBPPConfig <- QIBC.findByDomainAndCityAndVehicleCategory (show Spec.FRFS) merchantOperatingCity.id (frfsVehicleCategoryToBecknVehicleCategory searchReq.vehicleType) >>= return . fmap (.providerConfig)
  case (bapConfig.vehicleCategory, integratedBPPConfig) of
    (_, Nothing) -> do
      fork ("FRFS ONDC SearchReq for " <> show bapConfig.vehicleCategory) $ do
        fromStation <- QStation.findById searchReq.fromStationId >>= fromMaybeM (StationNotFound searchReq.fromStationId.getId)
        toStation <- QStation.findById searchReq.toStationId >>= fromMaybeM (StationNotFound searchReq.toStationId.getId)
        bknSearchReq <- ACL.buildSearchReq searchReq bapConfig fromStation toStation merchantOperatingCity.city
        logDebug $ "FRFS SearchReq " <> encodeToText bknSearchReq
        Metrics.startMetrics Metrics.SEARCH_FRFS merchant.name searchReq.id.getId merchantOperatingCity.id.getId
        void $ CallFRFSBPP.search bapConfig.gatewayUrl bknSearchReq merchant.id
    (METRO, Just config) -> do
      fork "FRFS External METRO SearchReq" $ do
        onSearchReq <- MetroFlow.search merchant merchantOperatingCity config bapConfig searchReq
        processOnSearch onSearchReq
    (BUS, Just _config) -> do
      fork "FRFS External SearchReq" $ do
        onSearchReq <- BusFlow.search merchant merchantOperatingCity bapConfig searchReq
        processOnSearch onSearchReq
    _ -> throwError $ InternalError ("Unsupported FRFS Flow vehicleCategory : " <> show bapConfig.vehicleCategory)
  where
    processOnSearch :: FRFSSearchFlow m r => DOnSearch.DOnSearch -> m ()
    processOnSearch onSearchReq = do
      validatedDOnSearch <- DOnSearch.validateRequest onSearchReq
      DOnSearch.onSearch onSearchReq validatedDOnSearch

init :: FRFSConfirmFlow m r => Merchant -> MerchantOperatingCity -> BecknConfig -> (Maybe Text, Maybe Text) -> DBooking.FRFSTicketBooking -> m ()
init merchant merchantOperatingCity bapConfig (mRiderName, mRiderNumber) booking = do
  integratedBPPConfig <- QIBC.findByDomainAndCityAndVehicleCategory (show Spec.FRFS) merchantOperatingCity.id (frfsVehicleCategoryToBecknVehicleCategory booking.vehicleType) >>= return . fmap (.providerConfig)
  case (bapConfig.vehicleCategory, integratedBPPConfig) of
    (METRO, Nothing) -> do
      providerUrl <- booking.bppSubscriberUrl & parseBaseUrl & fromMaybeM (InvalidRequest "Invalid provider url")
      bknInitReq <- ACL.buildInitReq (mRiderName, mRiderNumber) booking bapConfig Utils.BppData {bppId = booking.bppSubscriberId, bppUri = booking.bppSubscriberUrl} merchantOperatingCity.city
      logDebug $ "FRFS InitReq " <> encodeToText bknInitReq
      Metrics.startMetrics Metrics.INIT_FRFS merchant.name booking.searchId.getId merchantOperatingCity.id.getId
      void $ CallFRFSBPP.init providerUrl bknInitReq merchant.id
    (METRO, Just config) -> do
      onInitReq <- MetroFlow.init merchant merchantOperatingCity config bapConfig (mRiderName, mRiderNumber) booking
      processOnInit onInitReq
    (BUS, Just _config) -> do
      onInitReq <- BusFlow.init merchant merchantOperatingCity bapConfig (mRiderName, mRiderNumber) booking
      processOnInit onInitReq
    _ -> throwError $ InternalError ("Unsupported FRFS Flow vehicleCategory : " <> show bapConfig.vehicleCategory)
  where
    processOnInit :: FRFSConfirmFlow m r => DOnInit.DOnInit -> m ()
    processOnInit onInitReq = do
      (merchant', booking') <- DOnInit.validateRequest onInitReq
      DOnInit.onInit onInitReq merchant' booking'

cancel :: Merchant -> MerchantOperatingCity -> BecknConfig -> Spec.CancellationType -> DBooking.FRFSTicketBooking -> Flow ()
cancel merchant merchantOperatingCity bapConfig cancellationType booking = do
  integratedBPPConfig <- QIBC.findByDomainAndCityAndVehicleCategory (show Spec.FRFS) merchantOperatingCity.id (frfsVehicleCategoryToBecknVehicleCategory booking.vehicleType) >>= return . fmap (.providerConfig)
  case (bapConfig.vehicleCategory, integratedBPPConfig) of
    (METRO, Nothing) -> do
      fork "FRFS ONDC Cancel Req" $ do
        frfsConfig <-
          CQFRFSConfig.findByMerchantOperatingCityId merchantOperatingCity.id
            >>= fromMaybeM (InternalError $ "FRFS config not found for merchant operating city Id " <> merchantOperatingCity.id.getId)
        providerUrl <- booking.bppSubscriberUrl & parseBaseUrl & fromMaybeM (InvalidRequest "Invalid provider url")
        ttl <- bapConfig.cancelTTLSec & fromMaybeM (InternalError "Invalid ttl")
        when (cancellationType == Spec.CONFIRM_CANCEL) $ Redis.setExp (DOnCancel.makecancelledTtlKey booking.id) True ttl
        bknCancelReq <- ACL.buildCancelReq booking bapConfig Utils.BppData {bppId = booking.bppSubscriberId, bppUri = booking.bppSubscriberUrl} frfsConfig.cancellationReasonId cancellationType merchantOperatingCity.city
        logDebug $ "FRFS CancelReq " <> encodeToText bknCancelReq
        void $ CallFRFSBPP.cancel providerUrl bknCancelReq merchant.id
    (METRO, Just _config) -> return ()
    (BUS, Just _config) -> return ()
    _ -> throwError $ InternalError ("Unsupported FRFS Flow vehicleCategory : " <> show bapConfig.vehicleCategory)

confirm :: (DOrder -> Flow ()) -> Merchant -> MerchantOperatingCity -> BecknConfig -> (Maybe Text, Maybe Text) -> DBooking.FRFSTicketBooking -> Flow ()
confirm onConfirmHandler merchant merchantOperatingCity bapConfig (mRiderName, mRiderNumber) booking = do
  integratedBPPConfig <- QIBC.findByDomainAndCityAndVehicleCategory (show Spec.FRFS) merchantOperatingCity.id (frfsVehicleCategoryToBecknVehicleCategory booking.vehicleType) >>= return . fmap (.providerConfig)
  case (bapConfig.vehicleCategory, integratedBPPConfig) of
    (METRO, Nothing) -> do
      fork "FRFS ONDC Confirm Req" $ do
        providerUrl <- booking.bppSubscriberUrl & parseBaseUrl & fromMaybeM (InvalidRequest "Invalid provider url")
        bknConfirmReq <- ACL.buildConfirmReq (mRiderName, mRiderNumber) booking bapConfig booking.searchId.getId Utils.BppData {bppId = booking.bppSubscriberId, bppUri = booking.bppSubscriberUrl} merchantOperatingCity.city
        logDebug $ "FRFS ConfirmReq " <> encodeToText bknConfirmReq
        Metrics.startMetrics Metrics.CONFIRM_FRFS merchant.name booking.searchId.getId merchantOperatingCity.id.getId
        void $ CallFRFSBPP.confirm providerUrl bknConfirmReq merchant.id
    (METRO, Just config) -> do
      fork "FRFS EBIX Confirm Req" $ do
        frfsConfig <-
          CQFRFSConfig.findByMerchantOperatingCityId merchantOperatingCity.id
            >>= fromMaybeM (InternalError $ "FRFS config not found for merchant operating city Id " <> merchantOperatingCity.id.getId)
        onConfirmReq <- MetroFlow.confirm merchant merchantOperatingCity frfsConfig config bapConfig (mRiderName, mRiderNumber) booking
        onConfirmHandler onConfirmReq
    (BUS, Just config) -> do
      fork "FRFS EBIX Confirm Req" $ do
        frfsConfig <-
          CQFRFSConfig.findByMerchantOperatingCityId merchantOperatingCity.id
            >>= fromMaybeM (InternalError $ "FRFS config not found for merchant operating city Id " <> merchantOperatingCity.id.getId)
        onConfirmReq <- BusFlow.confirm merchant merchantOperatingCity frfsConfig config bapConfig (mRiderName, mRiderNumber) booking
        onConfirmHandler onConfirmReq
    _ -> throwError $ InternalError ("Unsupported FRFS Flow vehicleCategory : " <> show bapConfig.vehicleCategory)

status :: Id Merchant -> MerchantOperatingCity -> BecknConfig -> DBooking.FRFSTicketBooking -> Flow ()
status merchantId merchantOperatingCity bapConfig booking = do
  integratedBPPConfig <- QIBC.findByDomainAndCityAndVehicleCategory (show Spec.FRFS) merchantOperatingCity.id (frfsVehicleCategoryToBecknVehicleCategory booking.vehicleType) >>= return . fmap (.providerConfig)
  case (bapConfig.vehicleCategory, integratedBPPConfig) of
    (METRO, Nothing) -> do
      void $ CallFRFSBPP.callBPPStatus booking bapConfig merchantOperatingCity.city merchantId
    (METRO, Just config) -> do
      onStatusReq <- MetroFlow.status merchantId merchantOperatingCity config bapConfig booking
      processOnStatus onStatusReq
    (BUS, Just config) -> do
      onStatusReq <- BusFlow.status merchantId merchantOperatingCity config bapConfig booking
      processOnStatus onStatusReq
    _ -> throwError $ InternalError ("Unsupported FRFS Flow vehicleCategory : " <> show bapConfig.vehicleCategory)
  where
    processOnStatus onStatusReq = do
      let bookingOnStatusReq = DOnStatus.Booking onStatusReq
      (merchant', booking') <- DOnStatus.validateRequest bookingOnStatusReq
      DOnStatus.onStatus merchant' booking' bookingOnStatusReq

verifyTicket :: Id Merchant -> MerchantOperatingCity -> BecknConfig -> Spec.VehicleCategory -> Text -> Flow ()
verifyTicket merchantId merchantOperatingCity bapConfig vehicleCategory encryptedQrData = do
  integratedBPPConfig <- QIBC.findByDomainAndCityAndVehicleCategory (show Spec.FRFS) merchantOperatingCity.id (frfsVehicleCategoryToBecknVehicleCategory vehicleCategory) >>= return . fmap (.providerConfig)
  case (bapConfig.vehicleCategory, integratedBPPConfig) of
    (BUS, Just config) -> do
      onStatusReq <- BusFlow.verifyTicket merchantId merchantOperatingCity config bapConfig encryptedQrData
      processOnStatus onStatusReq
    _ -> throwError $ InternalError ("Unsupported FRFS Flow vehicleCategory : " <> show bapConfig.vehicleCategory)
  where
    processOnStatus onStatusReq = do
      let verificationOnStatusReq = DOnStatus.TicketVerification onStatusReq
      (merchant', booking') <- DOnStatus.validateRequest verificationOnStatusReq
      DOnStatus.onStatus merchant' booking' verificationOnStatusReq
