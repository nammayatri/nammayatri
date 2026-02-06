module ExternalBPP.CallAPI.Search where

import qualified API.Types.UI.FRFSTicketService as API
import qualified Beckn.ACL.FRFS.Search as ACL
import qualified BecknV2.FRFS.Enums as Spec
import BecknV2.FRFS.Utils
import qualified Domain.Action.Beckn.FRFS.OnSearch as DOnSearch
import Domain.Types.BecknConfig
import Domain.Types.FRFSQuote as DFRFSQuote
import Domain.Types.FRFSRouteDetails
import Domain.Types.FRFSSearch as DSearch
import Domain.Types.IntegratedBPPConfig
import Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Station as DStation
import ExternalBPP.CallAPI.Types
import qualified ExternalBPP.Flow as Flow
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Utils.Common
import qualified SharedLogic.CallFRFSBPP as CallFRFSBPP
import qualified SharedLogic.IntegratedBPPConfig as SIBC
import qualified Storage.CachedQueries.OTPRest.OTPRest as OTPRest
import Tools.Error
import qualified Tools.Metrics as Metrics

discoverySearch :: FRFSSearchFlow m r => Merchant -> BecknConfig -> IntegratedBPPConfig -> API.FRFSDiscoverySearchAPIReq -> m ()
discoverySearch merchant bapConfig integratedBPPConfig req = do
  transactionId <- generateGUID
  let requestCity = SIBC.resolveOndcCity integratedBPPConfig req.city
  bknSearchReq <- ACL.buildSearchReq transactionId req.vehicleType bapConfig Nothing Nothing requestCity
  let redisCounter = DOnSearch.DiscoveryCounter {merchantId = merchant.id.getId, pageNo = 0, maxPageNo = 9999}
  Redis.setExp (discoverySearchCounterKey (show transactionId)) redisCounter 3600
  logDebug $ "FRFS Discovery SearchReq " <> encodeToText bknSearchReq
  case integratedBPPConfig.providerConfig of
    ONDC ONDCBecknConfig {networkHostUrl} -> do
      let providerUrl = fromMaybe bapConfig.gatewayUrl networkHostUrl
      void $ CallFRFSBPP.search providerUrl bknSearchReq merchant.id
    _ -> do
      void $ CallFRFSBPP.search bapConfig.gatewayUrl bknSearchReq merchant.id

search :: (FRFSSearchFlow m r, HasShortDurationRetryCfg r c) => Merchant -> MerchantOperatingCity -> BecknConfig -> DSearch.FRFSSearch -> Maybe HighPrecMoney -> [FRFSRouteDetails] -> IntegratedBPPConfig -> [Spec.ServiceTierType] -> [DFRFSQuote.FRFSQuoteType] -> Bool -> m ()
search merchant merchantOperatingCity bapConfig searchReq mbFare routeDetails integratedBPPConfig blacklistedServiceTiers blacklistedFareQuoteTypes isSingleMode = do
  Metrics.startMetrics Metrics.SEARCH_FRFS merchant.name searchReq.id.getId merchantOperatingCity.id.getId
  case integratedBPPConfig.providerConfig of
    ONDC ONDCBecknConfig {networkHostUrl, networkId, fareCachingAllowed} -> do
      fork ("FRFS ONDC SearchReq for " <> show bapConfig.vehicleCategory) $ do
        case (fareCachingAllowed, networkHostUrl, networkId, mbFare) of
          (Just True, Just _, Just _, Just _) ->
            withTryCatch "callExternalBPP:searchFlow" (Flow.search merchant merchantOperatingCity integratedBPPConfig bapConfig networkHostUrl networkId searchReq routeDetails blacklistedServiceTiers blacklistedFareQuoteTypes isSingleMode)
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
      onSearchReq <- Flow.search merchant merchantOperatingCity integratedBPPConfig bapConfig Nothing Nothing searchReq routeDetails blacklistedServiceTiers blacklistedFareQuoteTypes isSingleMode
      processOnSearch onSearchReq
  where
    processOnSearch :: (FRFSSearchFlow m r, HasShortDurationRetryCfg r c) => DOnSearch.DOnSearch -> m ()
    processOnSearch onSearchReq = do
      validatedDOnSearch <- DOnSearch.validateRequest onSearchReq
      DOnSearch.onSearch onSearchReq validatedDOnSearch

    callOndcSearch :: Maybe BaseUrl -> (FRFSSearchFlow m r, HasShortDurationRetryCfg r c) => m ()
    callOndcSearch networkHostUrl = do
      let providerUrl = fromMaybe bapConfig.gatewayUrl networkHostUrl
          cityForOndc = SIBC.resolveOndcCity integratedBPPConfig merchantOperatingCity.city
      fromStation <- OTPRest.getStationByGtfsIdAndStopCode searchReq.fromStationCode integratedBPPConfig >>= fromMaybeM (StationNotFound searchReq.fromStationCode)
      toStation <- OTPRest.getStationByGtfsIdAndStopCode searchReq.toStationCode integratedBPPConfig >>= fromMaybeM (StationNotFound searchReq.toStationCode)
      routeStopMappingFromStation <- OTPRest.getRouteStopMappingByStopCode searchReq.fromStationCode integratedBPPConfig
      routeStopMappingToStation <- OTPRest.getRouteStopMappingByStopCode searchReq.toStationCode integratedBPPConfig
      let fromStationProviderCode = fromMaybe searchReq.fromStationCode (listToMaybe routeStopMappingFromStation <&> (.providerCode))
          toStationProviderCode = fromMaybe searchReq.toStationCode (listToMaybe routeStopMappingToStation <&> (.providerCode))
      bknSearchReq <- ACL.buildSearchReq searchReq.id.getId searchReq.vehicleType bapConfig (Just $ fromStation {DStation.code = fromStationProviderCode}) (Just $ toStation {DStation.code = toStationProviderCode}) cityForOndc
      logDebug $ "FRFS SearchReq " <> encodeToText bknSearchReq
      void $ CallFRFSBPP.search providerUrl bknSearchReq merchant.id
