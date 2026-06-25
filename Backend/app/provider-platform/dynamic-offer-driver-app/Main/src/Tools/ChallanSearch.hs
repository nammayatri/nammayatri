module Tools.ChallanSearch where

import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.MerchantServiceConfig as DMSC
import qualified Kernel.External.ChallanSearch.Interface as ChallanSearch
import qualified Kernel.External.ChallanSearch.Interface.Types as ChallanSearchTypes
import qualified Kernel.External.ChallanSearch.Types as ChallanSearchServiceTypes
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import qualified Storage.Cac.MerchantServiceUsageConfig as CQMSUC
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as CQMSC
import Storage.ConfigPilot.Config.MerchantServiceConfig (MerchantServiceConfigDimensions (..))
import Storage.ConfigPilot.Config.MerchantServiceUsageConfig (MerchantServiceUsageConfigDimensions (..))

getChallanProvidersPriorityList ::
  ServiceFlow m r =>
  Id DMOC.MerchantOperatingCity ->
  m (Maybe [ChallanSearchServiceTypes.ChallanSearchService])
getChallanProvidersPriorityList merchantOpCityId = do
  mbUsageConfig <- getOneConfig (MerchantServiceUsageConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId}) (Just (CQMSUC.findByMerchantOpCityId merchantOpCityId Nothing))
  pure $ mbUsageConfig >>= (.challanProvidersPriorityList)

getServiceConfig ::
  ServiceFlow m r =>
  Id DMOC.MerchantOperatingCity ->
  ChallanSearchServiceTypes.ChallanSearchService ->
  m ChallanSearchTypes.ChallanSearchServiceConfig
getServiceConfig merchantOpCityId provider = do
  merchantServiceConfig <-
    getOneConfig (MerchantServiceConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId, merchantId = Nothing, serviceName = Just (DMSC.ChallanSearchService provider)}) (Just (maybeToList <$> CQMSC.findByServiceAndCity (DMSC.ChallanSearchService provider) merchantOpCityId))
      >>= fromMaybeM (MerchantServiceConfigNotFound merchantOpCityId.getId "ChallanSearch" (show provider))
  case merchantServiceConfig.serviceConfig of
    DMSC.ChallanSearchServiceConfig cfg -> pure cfg
    _ -> throwError $ InternalError "Unknown ChallanSearch Service Config"

getPendingChallanCount ::
  ServiceFlow m r =>
  Id DMOC.MerchantOperatingCity ->
  ChallanSearchTypes.PendingChallanReq ->
  m ChallanSearchTypes.PendingChallanResp
getPendingChallanCount merchantOpCityId req = do
  mbProviders <- getChallanProvidersPriorityList merchantOpCityId
  case mbProviders of
    Just providers@(_ : _) ->
      ChallanSearch.getPendingChallanCountWithFallback (getServiceConfig merchantOpCityId) providers req
    _ -> throwError $ InternalError "No challan search provider configured"
