{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Tools.Auth.Merchant (merchantCityAccessCheck, merchantServerAccessCheck, CheckedShortId, skipMerchantCityAccessCheck) where

import qualified Domain.Types.Merchant as DMerchant
import qualified Domain.Types.ServerName as DTServer
import Kernel.Prelude
import Kernel.Types.Beckn.City as City
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant (ToHttpApiData)

-- import Kernel.Utils.Logging(logDebug)

merchantCityAccessCheck :: MonadFlow m => ShortId DMerchant.Merchant -> ShortId DMerchant.Merchant -> City.City -> City.City -> m (CheckedShortId DMerchant.Merchant)
merchantCityAccessCheck (ShortId userMerchantId) (ShortId merchantId) userCity city = do
  logDebug $ "Data: " <> show userMerchantId <> " " <> show merchantId <> " " <> show userCity <> " " <> show city
  unless (userMerchantId == merchantId && userCity == city) $ throwError AccessDenied
  pure $ CheckedShortId merchantId

skipMerchantCityAccessCheck :: ShortId DMerchant.Merchant -> CheckedShortId DMerchant.Merchant
skipMerchantCityAccessCheck (ShortId merchantId) = CheckedShortId merchantId

-- CheckedShortId constructor should not be exported for type safety
newtype CheckedShortId domain = CheckedShortId Text
  deriving newtype (ToHttpApiData, ToSchema)

merchantServerAccessCheck ::
  ( MonadFlow m,
    HasFlowEnv m r '["dataServers" ::: [DTServer.DataServer]]
  ) =>
  DMerchant.Merchant ->
  m ()
merchantServerAccessCheck merchant = do
  availableServers <- asks (.dataServers)
  unless (all (`elem` (availableServers <&> (.name))) merchant.serverNames) $
    throwError $ InvalidRequest "Server for this merchant is not available"
