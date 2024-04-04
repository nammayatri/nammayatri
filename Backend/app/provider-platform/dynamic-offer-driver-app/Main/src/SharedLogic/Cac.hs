{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module SharedLogic.Cac where

import qualified Data.Aeson as DA
import Domain.Types.Merchant.MerchantOperatingCity
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as QMerchantOpCity
import Tools.Error
import Utils.Common.CacUtils

getFrontendConfigs :: (KvDbFlow m r, Log m) => Id MerchantOperatingCity -> Maybe Int -> m (Maybe DA.Object)
getFrontendConfigs merchantOpCityId mbToss = do
  city <- (QMerchantOpCity.findById merchantOpCityId >>= fromMaybeM (MerchantOperatingCityNotFound merchantOpCityId.getId)) <&> (.city)
  let ghcCond = [(City, show city)]
  contextValue <- case mbToss of
    Just toss -> getConfigFromCac ghcCond (show DriverFrontEndTenat) toss Empty
    Nothing -> getConfigFromCac ghcCond (show DriverFrontEndTenat) 1 Empty
  case contextValue of
    Nothing -> do
      logError $ "Error in getting frontend configs for City: " <> show city <> "toss: " <> show mbToss
      return Nothing
    Just cfgs -> return $ Just cfgs
