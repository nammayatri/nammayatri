{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Tools.Meta (lookupMetaCfg) where

import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.MerchantServiceConfig as DMSC
import Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.External.Meta.Config as Meta
import Kernel.Types.Error
import qualified Kernel.Types.Id as KId
import Kernel.Utils.Common
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as CQMSC

-- | Fetch the (encrypted) Meta WhatsApp Cloud API config for a merchant-city
-- from its @merchant_service_config@ row (mirrors Xyne's @lookupXyneCfg@). The
-- access token stays @EncryptedField@-encrypted here; only
-- @Kernel.External.Meta.Flow.sendMessage@ decrypts it, at call time. Webhook
-- secrets (verifyToken\/appSecret) live in app-env, not this row.
lookupMetaCfg :: KId.Id DM.Merchant -> KId.Id DMOC.MerchantOperatingCity -> Flow Meta.MetaCfg
lookupMetaCfg merchantId mocId = do
  msc <-
    CQMSC.findByMerchantOpCityIdAndService merchantId mocId (DMSC.MetaService DMSC.CloudApi)
      >>= fromMaybeM (InternalError $ "Meta config not found for merchantOperatingCityId: " <> mocId.getId)
  case msc.serviceConfig of
    DMSC.MetaServiceConfig cfg -> pure cfg
    _ -> throwError (InternalError "Unexpected service config shape for Meta")
