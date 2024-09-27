module ExternalBPP.Common.Search where

import qualified Beckn.ACL.FRFS.Search as ACL
import BecknV2.OnDemand.Enums
import qualified Domain.Action.Beckn.FRFS.OnSearch as DOnSearch
import Domain.Types.BecknConfig
import Domain.Types.FRFSSearch as DSearch
import Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity
import Environment
import qualified ExternalBPP.EBIX.Flow as EBIXFlow
import Kernel.Prelude
import Kernel.Utils.Common
import qualified SharedLogic.CallFRFSBPP as CallFRFSBPP
import qualified Storage.CachedQueries.Station as QStation
import Tools.Error
import qualified Tools.Metrics as Metrics

search :: Merchant -> MerchantOperatingCity -> BecknConfig -> DSearch.FRFSSearch -> Flow ()
search merchant merchantOperatingCity bapConfig searchReq = do
  case (bapConfig.vehicleCategory, bapConfig.provider) of
    (METRO, ONDC) -> do
      fork "FRFS ONDC SearchReq" $ do
        fromStation <- QStation.findById searchReq.fromStationId >>= fromMaybeM (StationNotFound searchReq.fromStationId.getId)
        toStation <- QStation.findById searchReq.toStationId >>= fromMaybeM (StationNotFound searchReq.toStationId.getId)
        bknSearchReq <- ACL.buildSearchReq searchReq bapConfig fromStation toStation merchantOperatingCity.city
        logDebug $ "FRFS SearchReq " <> encodeToText bknSearchReq
        Metrics.startMetrics Metrics.SEARCH_FRFS merchant.name searchReq.id.getId merchantOperatingCity.id.getId
        void $ CallFRFSBPP.search bapConfig.gatewayUrl bknSearchReq merchant.id
    (BUS, EBIX) -> do
      fork "FRFS EBIX SearchReq" $ do
        onSearchReq <- EBIXFlow.search merchant merchantOperatingCity bapConfig searchReq
        processOnSearch onSearchReq
    _ -> throwError $ InternalError ("Unsupported FRFS Flow vehicleCategory : " <> show bapConfig.vehicleCategory <> ", provider: " <> show bapConfig.provider)
  where
    processOnSearch onSearchReq = do
      validatedDOnSearch <- DOnSearch.validateRequest onSearchReq
      DOnSearch.onSearch onSearchReq validatedDOnSearch
