module ExternalBPP.CallAPI
  ( module ExternalBPP.CallAPI,
    module Reexport,
  )
where

import qualified Beckn.ACL.FRFS.Confirm as ACL
import qualified Beckn.ACL.FRFS.Init as ACL
import qualified Beckn.ACL.FRFS.Search as ACL
import qualified Beckn.ACL.FRFS.Utils as Utils
import qualified BecknV2.FRFS.Enums as Spec
import BecknV2.FRFS.Utils
import BecknV2.OnDemand.Enums
import qualified Domain.Action.Beckn.FRFS.OnConfirm as DOnConfirm
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
import ExternalBPP.Common.Cancel as Reexport
import qualified ExternalBPP.Metro.Flow as MetroFlow
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.CallFRFSBPP as CallFRFSBPP
import qualified Storage.CachedQueries.FRFSConfig as CQFRFSConfig
import Storage.CachedQueries.IntegratedBPPConfig as QIBC
import qualified Storage.CachedQueries.Station as QStation
import Tools.Error
import qualified Tools.Metrics as Metrics

search :: Merchant -> MerchantOperatingCity -> BecknConfig -> DSearch.FRFSSearch -> Flow ()
search merchant merchantOperatingCity bapConfig searchReq = do
  integratedBPPConfig <- QIBC.findByDomainAndCityAndVehicleCategory (show Spec.FRFS) merchantOperatingCity.id (frfsVehicleCategoryToBecknVehicleCategory searchReq.vehicleType) >>= return . fmap (.providerConfig)
  case (bapConfig.vehicleCategory, integratedBPPConfig) of
    (METRO, Nothing) -> do
      fork "FRFS ONDC METRO SearchReq" $ do
        fromStation <- QStation.findById searchReq.fromStationId >>= fromMaybeM (StationNotFound searchReq.fromStationId.getId)
        toStation <- QStation.findById searchReq.toStationId >>= fromMaybeM (StationNotFound searchReq.toStationId.getId)
        bknSearchReq <- ACL.buildSearchReq searchReq bapConfig fromStation toStation merchantOperatingCity.city
        logDebug $ "FRFS SearchReq " <> encodeToText bknSearchReq
        Metrics.startMetrics Metrics.SEARCH_FRFS merchant.name searchReq.id.getId merchantOperatingCity.id.getId
        void $ CallFRFSBPP.search bapConfig.gatewayUrl bknSearchReq merchant.id
    (METRO, Just _config) -> do
      fork "FRFS External METRO SearchReq" $ do
        onSearchReq <- MetroFlow.search merchant merchantOperatingCity bapConfig searchReq
        processOnSearch onSearchReq
    (BUS, Just _config) -> do
      fork "FRFS External SearchReq" $ do
        onSearchReq <- BusFlow.search merchant merchantOperatingCity bapConfig searchReq
        processOnSearch onSearchReq
    _ -> throwError $ InternalError ("Unsupported FRFS Flow vehicleCategory : " <> show bapConfig.vehicleCategory)
  where
    processOnSearch onSearchReq = do
      validatedDOnSearch <- DOnSearch.validateRequest onSearchReq
      DOnSearch.onSearch onSearchReq validatedDOnSearch

init :: Merchant -> MerchantOperatingCity -> BecknConfig -> (Maybe Text, Maybe Text) -> DBooking.FRFSTicketBooking -> Flow ()
init merchant merchantOperatingCity bapConfig (mRiderName, mRiderNumber) booking = do
  integratedBPPConfig <- QIBC.findByDomainAndCityAndVehicleCategory (show Spec.FRFS) merchantOperatingCity.id (frfsVehicleCategoryToBecknVehicleCategory booking.vehicleType) >>= return . fmap (.providerConfig)
  case (bapConfig.vehicleCategory, integratedBPPConfig) of
    (METRO, Nothing) -> do
      providerUrl <- booking.bppSubscriberUrl & parseBaseUrl & fromMaybeM (InvalidRequest "Invalid provider url")
      bknInitReq <- ACL.buildInitReq (mRiderName, mRiderNumber) booking bapConfig Utils.BppData {bppId = booking.bppSubscriberId, bppUri = booking.bppSubscriberUrl} merchantOperatingCity.city
      logDebug $ "FRFS InitReq " <> encodeToText bknInitReq
      Metrics.startMetrics Metrics.INIT_FRFS merchant.name booking.searchId.getId merchantOperatingCity.id.getId
      void $ CallFRFSBPP.init providerUrl bknInitReq merchant.id
    (METRO, Just _config) -> do
      onInitReq <- MetroFlow.init merchant merchantOperatingCity bapConfig (mRiderName, mRiderNumber) booking
      processOnInit onInitReq
    (BUS, Just _config) -> do
      onInitReq <- BusFlow.init merchant merchantOperatingCity bapConfig (mRiderName, mRiderNumber) booking
      processOnInit onInitReq
    _ -> throwError $ InternalError ("Unsupported FRFS Flow vehicleCategory : " <> show bapConfig.vehicleCategory)
  where
    processOnInit onInitReq = do
      (merchant', booking') <- DOnInit.validateRequest onInitReq
      DOnInit.onInit onInitReq merchant' booking'

confirm :: Merchant -> MerchantOperatingCity -> BecknConfig -> (Maybe Text, Maybe Text) -> DBooking.FRFSTicketBooking -> Flow ()
confirm merchant merchantOperatingCity bapConfig (mRiderName, mRiderNumber) booking = do
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
        processOnConfirm onConfirmReq
    (BUS, Just config) -> do
      fork "FRFS EBIX Confirm Req" $ do
        frfsConfig <-
          CQFRFSConfig.findByMerchantOperatingCityId merchantOperatingCity.id
            >>= fromMaybeM (InternalError $ "FRFS config not found for merchant operating city Id " <> merchantOperatingCity.id.getId)
        onConfirmReq <- BusFlow.confirm merchant merchantOperatingCity frfsConfig config bapConfig (mRiderName, mRiderNumber) booking
        processOnConfirm onConfirmReq
    _ -> throwError $ InternalError ("Unsupported FRFS Flow vehicleCategory : " <> show bapConfig.vehicleCategory)
  where
    processOnConfirm onConfirmReq = do
      (merchant', booking') <- DOnConfirm.validateRequest onConfirmReq
      DOnConfirm.onConfirm merchant' booking' onConfirmReq

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
      (merchant', booking') <- DOnStatus.validateRequest onStatusReq
      DOnStatus.onStatus merchant' booking' onStatusReq
