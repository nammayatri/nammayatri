module ExternalBPP.CallAPI where

import qualified API.Types.UI.FRFSTicketService
import qualified Beckn.ACL.FRFS.Cancel as ACL
import qualified Beckn.ACL.FRFS.Confirm as ACL
import qualified Beckn.ACL.FRFS.Init as ACL
import qualified Beckn.ACL.FRFS.Search as ACL
import qualified Beckn.ACL.FRFS.Utils as Utils
import qualified BecknV2.FRFS.Enums as Spec
import BecknV2.FRFS.Utils
import Domain.Action.Beckn.FRFS.Common hiding (status)
import qualified Domain.Action.Beckn.FRFS.OnCancel as DOnCancel
import qualified Domain.Action.Beckn.FRFS.OnInit as DOnInit
import qualified Domain.Action.Beckn.FRFS.OnSearch as DOnSearch
import qualified Domain.Action.Beckn.FRFS.OnStatus as DOnStatus
import Domain.Types.BecknConfig
import Domain.Types.FRFSRouteDetails
import Domain.Types.FRFSSearch as DSearch
import qualified Domain.Types.FRFSTicketBooking as DBooking
import Domain.Types.IntegratedBPPConfig
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Route as DRoute
import Environment
import EulerHS.Prelude ((+||), (||+))
import qualified ExternalBPP.Flow as Flow
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import qualified Kernel.Types.Registry as DReg
import Kernel.Utils.Common
import qualified Kernel.Utils.Registry as Registry
import Lib.Payment.Storage.Beam.BeamFlow
import qualified SharedLogic.CallFRFSBPP as CallFRFSBPP
import qualified SharedLogic.FRFSUtils as FRFSUtils
import qualified Storage.CachedQueries.FRFSConfig as CQFRFSConfig
import Storage.CachedQueries.IntegratedBPPConfig as QIBC
import qualified Storage.CachedQueries.Station as QStation
import qualified Storage.Queries.RouteStopMapping as QRouteStopMapping
import Tools.Error
import qualified Tools.Metrics as Metrics

type FRFSSearchFlow m r =
  ( CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    Metrics.HasBAPMetrics m r,
    CallFRFSBPP.BecknAPICallFlow m r,
    EncFlow m r,
    HasField "ondcRegistryUrl" r BaseUrl
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

discoverySearch :: FRFSSearchFlow m r => Merchant -> BecknConfig -> API.Types.UI.FRFSTicketService.FRFSDiscoverySearchAPIReq -> m ()
discoverySearch merchant bapConfig req = do
  transactionId <- generateGUID
  bknSearchReq <- ACL.buildSearchReq transactionId req.vehicleType bapConfig Nothing Nothing req.city
  logDebug $ "FRFS Discovery SearchReq " <> encodeToText bknSearchReq
  void $ CallFRFSBPP.search bapConfig.gatewayUrl bknSearchReq merchant.id

search :: FRFSSearchFlow m r => Merchant -> MerchantOperatingCity -> BecknConfig -> DSearch.FRFSSearch -> [FRFSRouteDetails] -> IntegratedBPPConfig -> m ()
search merchant merchantOperatingCity bapConfig searchReq routeDetails integratedBPPConfig = do
  case integratedBPPConfig.providerConfig of
    ONDC _ -> do
      fork ("FRFS ONDC SearchReq for " <> show bapConfig.vehicleCategory) $ do
        fromStation <- QStation.findById searchReq.fromStationId >>= fromMaybeM (StationNotFound searchReq.fromStationId.getId)
        toStation <- QStation.findById searchReq.toStationId >>= fromMaybeM (StationNotFound searchReq.toStationId.getId)
        routeInfo' <- FRFSUtils.getRouteByStationIdsAndIntegratedBPPConfigId searchReq.fromStationId searchReq.toStationId (Just integratedBPPConfig.id)
        case routeInfo' of
          Just routeInfo -> do
            fromRoute <- (listToMaybe <$> QRouteStopMapping.findByRouteCodeAndStopCode routeInfo.route.code routeInfo.startStopCode integratedBPPConfig.id) >>= fromMaybeM (RouteMappingDoesNotExist routeInfo.route.code routeInfo.startStopCode integratedBPPConfig.id.getId)
            toRoute <- (listToMaybe <$> QRouteStopMapping.findByRouteCodeAndStopCode routeInfo.route.code routeInfo.endStopCode integratedBPPConfig.id) >>= fromMaybeM (RouteMappingDoesNotExist routeInfo.route.code routeInfo.endStopCode integratedBPPConfig.id.getId)
            bknSearchReq <- ACL.buildSearchReq searchReq.id.getId searchReq.vehicleType bapConfig (Just fromStation{code = fromRoute.providerCode}) (Just toStation{code = toRoute.providerCode}) merchantOperatingCity.city
            logDebug $ "FRFS SearchReq " <> encodeToText bknSearchReq
            let providerDetails = routeInfo.route.providerDetails
            case providerDetails of
              Just (DRoute.ONDC ondcProviderDetails) -> do
                ondcRegistryUrl <- asks (.ondcRegistryUrl)
                let lookupRequest =
                      DReg.SimpleLookupRequest
                        { unique_key_id = ondcProviderDetails.uniqueKeyId,
                          subscriber_id = ondcProviderDetails.subscriberId,
                          merchant_id = merchant.id.getId,
                          subscriber_type = DReg.BPP,
                          domain = Context.PUBLIC_TRANSPORT
                        }
                Registry.registryLookup ondcRegistryUrl lookupRequest bapConfig.subscriberId >>= \case
                  Just subscriber -> do
                    Metrics.startMetrics Metrics.SEARCH_FRFS merchant.name searchReq.id.getId merchantOperatingCity.id.getId
                    void $ CallFRFSBPP.search subscriber.subscriber_url bknSearchReq merchant.id
                  Nothing -> do
                    logWarning $
                      "Subscriber with unique_key_id:"
                        <> ondcProviderDetails.uniqueKeyId
                        <> "; subscriber type: "
                        <> show lookupRequest.subscriber_type
                        <> "; domain: "
                        <> show lookupRequest.domain
                        <> " not found."
                    throwError $ LookUpFailed ondcProviderDetails.subscriberId
              Nothing -> do
                bknSearchReq' <- ACL.buildSearchReq searchReq.id.getId searchReq.vehicleType bapConfig (Just fromStation) (Just toStation) merchantOperatingCity.city
                logDebug $ "FRFS SearchReq " <> encodeToText bknSearchReq'
                Metrics.startMetrics Metrics.SEARCH_FRFS merchant.name searchReq.id.getId merchantOperatingCity.id.getId
                void $ CallFRFSBPP.search bapConfig.gatewayUrl bknSearchReq' merchant.id
          Nothing -> do
            bknSearchReq <- ACL.buildSearchReq searchReq.id.getId searchReq.vehicleType bapConfig (Just fromStation) (Just toStation) merchantOperatingCity.city
            logDebug $ "FRFS SearchReq " <> encodeToText bknSearchReq
            Metrics.startMetrics Metrics.SEARCH_FRFS merchant.name searchReq.id.getId merchantOperatingCity.id.getId
            void $ CallFRFSBPP.search bapConfig.gatewayUrl bknSearchReq merchant.id
    _ -> do
      fork "FRFS External SearchReq" $ do
        onSearchReq <- Flow.search merchant merchantOperatingCity integratedBPPConfig bapConfig searchReq routeDetails
        processOnSearch onSearchReq
  where
    processOnSearch :: FRFSSearchFlow m r => DOnSearch.DOnSearch -> m ()
    processOnSearch onSearchReq = do
      validatedDOnSearch <- DOnSearch.validateRequest onSearchReq
      DOnSearch.onSearch onSearchReq validatedDOnSearch

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
        routeInfo <- FRFSUtils.getRouteByStationIdsAndIntegratedBPPConfigId booking.fromStationId booking.toStationId (Just integratedBPPConfig.id)
        let routeId = routeInfo <&> (.route.id)
        frfsConfig <-
          CQFRFSConfig.findByMerchantOperatingCityIdAndRouteId merchantOperatingCity.id routeId
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
        routeInfo <- FRFSUtils.getRouteByStationIdsAndIntegratedBPPConfigId booking.fromStationId booking.toStationId (Just integratedBPPConfig.id)
        let routeId = routeInfo <&> (.route.id)
        frfsConfig <-
          CQFRFSConfig.findByMerchantOperatingCityIdAndRouteId merchantOperatingCity.id routeId
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
      processOnStatus onStatusReq
  where
    processOnStatus onStatusReq = do
      let bookingOnStatusReq = DOnStatus.Booking onStatusReq
      (merchant', booking') <- DOnStatus.validateRequest bookingOnStatusReq
      DOnStatus.onStatus merchant' booking' bookingOnStatusReq

verifyTicket :: Id Merchant -> MerchantOperatingCity -> BecknConfig -> Spec.VehicleCategory -> Text -> DIBC.PlatformType -> Flow ()
verifyTicket merchantId merchantOperatingCity bapConfig vehicleCategory encryptedQrData platformType = do
  integratedBPPConfig <- QIBC.findByDomainAndCityAndVehicleCategory (show Spec.FRFS) merchantOperatingCity.id (frfsVehicleCategoryToBecknVehicleCategory vehicleCategory) platformType >>= fromMaybeM (IntegratedBPPConfigNotFound $ "MerchantOperatingCityId:" +|| merchantOperatingCity.id.getId ||+ "Domain:" +|| Spec.FRFS ||+ "Vehicle:" +|| frfsVehicleCategoryToBecknVehicleCategory vehicleCategory ||+ "Platform Type:" +|| platformType ||+ "")
  onStatusReq <- Flow.verifyTicket merchantId merchantOperatingCity integratedBPPConfig bapConfig encryptedQrData
  processOnStatus onStatusReq
  where
    processOnStatus onStatusReq = do
      let verificationOnStatusReq = DOnStatus.TicketVerification onStatusReq
      (merchant', booking') <- DOnStatus.validateRequest verificationOnStatusReq
      void $ status merchantId merchantOperatingCity bapConfig booking' platformType -- Doing one status call to sync the Ticket Status & Expiry Before Verifying
      DOnStatus.onStatus merchant' booking' verificationOnStatusReq
