{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Tools.DriverBackgroundVerification where

import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.MerchantServiceConfig as DMSC
import Domain.Types.MerchantServiceUsageConfig
import Kernel.External.Types (ServiceFlow)
import Kernel.External.Verification as Reexport
import qualified Kernel.External.Verification as Verification
import Kernel.External.Verification.SafetyPortal.Types
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import Storage.ConfigPilot.Config.MerchantServiceConfig (MerchantServiceConfigDimensions (..))
import Storage.ConfigPilot.Config.MerchantServiceUsageConfig (MerchantServiceUsageConfigDimensions (..))
import Tools.Error

searchAgent ::
  ServiceFlow m r =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Agent ->
  m SearchAgentResponse
searchAgent = runWithServiceConfig Verification.searchAgent (.driverBackgroundVerificationService)

runWithServiceConfig ::
  ServiceFlow m r =>
  (DriverBackgroundVerificationServiceConfig -> req -> m resp) ->
  (MerchantServiceUsageConfig -> DriverBackgroundVerificationService) ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  req ->
  m resp
runWithServiceConfig func getCfg merchantId merchantOpCityId req = do
  logDebug $ "runWithServiceConfig: merchantId: " <> merchantId.getId <> ", merchantOpCityId: " <> merchantOpCityId.getId <> "WE REACHED TILL HERE!"
  merchantServiceUsageConfig <-
    getOneConfig (MerchantServiceUsageConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId}) Nothing
      >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantOpCityId.getId)
  logDebug $ "runWithServiceConfig: merchantServiceUsageConfig: " <> show merchantServiceUsageConfig
  merchantServiceConfig <-
    getOneConfig (MerchantServiceConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId, merchantId = Nothing, serviceName = Just (DMSC.DriverBackgroundVerificationService $ getCfg merchantServiceUsageConfig)}) Nothing
      >>= fromMaybeM (InternalError $ "No verification service provider configured for the merchant, merchantOpCityId:" <> merchantOpCityId.getId)
  case merchantServiceConfig.serviceConfig of
    DMSC.DriverBackgroundVerificationServiceConfig vsc -> func vsc req
    _ -> throwError $ InternalError "Unknown Service Config"
