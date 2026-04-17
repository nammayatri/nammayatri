{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.

  You should have received a copy of the GNU Affero General Public License along with this program.
  If not, see <https://www.gnu.org/licenses/>.
-}

module Tools.Plasma
  ( getLMSModules,
    allLMSTrainingCompleted,
  )
where

import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.MerchantServiceConfig as DMSC
import qualified Kernel.External.Plasma as KernelPlasma
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.ConfigPilot.Config.MerchantServiceConfig as MSCD
import Storage.ConfigPilot.Interface.Types (getOneConfig)

getLMSModules ::
  ServiceFlow m r =>
  Id DMOC.MerchantOperatingCity ->
  Text ->
  m (Maybe KernelPlasma.LMSModulesResp)
getLMSModules merchantOpCityId driverIdText = do
  mbServiceConfig <- getOneConfig (MSCD.MerchantServiceConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId, serviceName = Just (DMSC.PlasmaService KernelPlasma.LMS)})
  case mbServiceConfig of
    Nothing -> return Nothing
    Just sc -> case sc.serviceConfig of
      DMSC.PlasmaServiceConfig plasmaConfig -> do
        result <- withTryCatch "getLMSModules" $ KernelPlasma.getLMSModules plasmaConfig driverIdText
        return $ either (const Nothing) Just result
      _ -> return Nothing

allLMSTrainingCompleted ::
  ServiceFlow m r =>
  Id DMOC.MerchantOperatingCity ->
  Text ->
  m (Maybe Bool)
allLMSTrainingCompleted merchantOpCityId driverIdText = do
  mbModules <- getLMSModules merchantOpCityId driverIdText
  return $ fmap (all (\lmsModule -> lmsModule.status == KernelPlasma.COMPLETED)) mbModules
