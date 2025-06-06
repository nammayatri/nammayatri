{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Tools.Payout where

-- import qualified Data.Text as T
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.MerchantServiceConfig as DMSC
-- import qualified EulerHS.Language as L
import qualified Kernel.External.Payout.Interface as Payout
import qualified Kernel.External.Payout.Types as PT
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Types.Version
import Kernel.Utils.Common
import Kernel.Utils.Version
import qualified Storage.Cac.TransporterConfig as SCTC
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as CQMSC

-- import qualified System.Environment as SE

createPayoutOrder :: (ServiceFlow m r, HasFlowEnv m r '["selfBaseUrl" ::: BaseUrl]) => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> DMSC.ServiceName -> Maybe Text -> Payout.CreatePayoutOrderReq -> m Payout.CreatePayoutOrderResp
createPayoutOrder = runWithServiceConfigAndName Payout.createPayoutOrder

payoutOrderStatus :: ServiceFlow m r => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> DMSC.ServiceName -> Maybe Text -> Payout.PayoutOrderStatusReq -> m Payout.PayoutOrderStatusResp
payoutOrderStatus = runWithServiceConfigAndName Payout.payoutOrderStatus

runWithServiceConfigAndName ::
  ServiceFlow m r =>
  (Payout.PayoutServiceConfig -> Maybe Text -> req -> m resp) ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  DMSC.ServiceName ->
  Maybe Text ->
  req ->
  m resp
runWithServiceConfigAndName func merchantId merchantOperatingCity serviceName mRoutingId req = do
  merchantServiceConfig <-
    CQMSC.findByServiceAndCity serviceName merchantOperatingCity
      >>= fromMaybeM (MerchantServiceConfigNotFound merchantId.getId "Payout" (show Payout.Juspay))
  case merchantServiceConfig.serviceConfig of
    DMSC.PayoutServiceConfig vsc -> func vsc getRoutingId req
    DMSC.RentalPayoutServiceConfig vsc -> func vsc getRoutingId req
    _ -> throwError $ InternalError "Unknown Service Config"
  where
    getRoutingId = do
      case serviceName of
        DMSC.PayoutService PT.AAJuspay -> mRoutingId
        _ -> Nothing

decidePayoutService :: (ServiceFlow m r) => DMSC.ServiceName -> Maybe Version -> Id DMOC.MerchantOperatingCity -> m DMSC.ServiceName
decidePayoutService payoutServiceName clientSdkVersion merchantOpCityId = do
  transporterConfig <- SCTC.findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  return $ case clientSdkVersion of
    Just v
      | v >= textToVersionDefault transporterConfig.aaEnabledClientSdkVersion -> DMSC.PayoutService PT.AAJuspay
    _ -> payoutServiceName
