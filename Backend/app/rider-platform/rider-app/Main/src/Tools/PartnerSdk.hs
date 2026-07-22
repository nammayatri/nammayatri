{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.

  You should have received a copy of the GNU Affero General Public License along with this program.
  If not, see <https://www.gnu.org/licenses/>.
-}

module Tools.PartnerSdk
  ( generateContributorToken,
  )
where

import qualified Domain.Types.Extra.MerchantServiceConfig as ExtraMSC
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.MerchantServiceConfig as DMSC
import qualified Kernel.External.PartnerSdk.Interface as PartnerSdk
import qualified Kernel.External.PartnerSdk.Interface.Types as PartnerSdk
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as CQMSC
import Storage.ConfigPilot.Config.MerchantServiceConfig (MerchantServiceConfigDimensions (..))

generateContributorToken ::
  (EncFlow m r, EsqDBFlow m r, CacheFlow m r) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  PartnerSdk.GenerateContributorTokenReq ->
  m PartnerSdk.GenerateContributorTokenResp
generateContributorToken merchantId merchantOpCityId req = do
  let serviceName = DMSC.PartnerSdkService ExtraMSC.Aarokya
  msc <-
    getOneConfig (MerchantServiceConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId, merchantId = merchantId.getId, serviceName = Just serviceName}) (Just (maybeToList <$> CQMSC.findByMerchantOpCityIdAndService merchantId merchantOpCityId serviceName))
      >>= fromMaybeM (InternalError $ "PartnerSdk Aarokya service config not found for merchantOpCityId: " <> merchantOpCityId.getId)
  case msc.serviceConfig of
    DMSC.PartnerSdkServiceConfig cfg -> PartnerSdk.generateContributorToken cfg req
    _ -> throwError $ InternalError "Unexpected service config shape for PartnerSdkService Aarokya"
