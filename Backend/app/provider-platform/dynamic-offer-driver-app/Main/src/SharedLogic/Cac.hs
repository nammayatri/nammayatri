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
import qualified Data.Aeson.KeyMap as DAKM
import qualified Data.Text
import Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import Kernel.Types.CacheFlow
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Error.Throwing
import Kernel.Utils.Logging
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as QMerchantOpCity
import Tools.Error
import Utils.Common.CacUtils

getFrontendConfigs ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r, Log m) =>
  Id MerchantOperatingCity ->
  Maybe Int ->
  Maybe Text ->
  DA.Object ->
  m (Maybe DA.Object)
getFrontendConfigs merchantOpCityId toss tenant context = do
  city <- (QMerchantOpCity.findById merchantOpCityId >>= fromMaybeM (MerchantOperatingCityNotFound merchantOpCityId.getId)) <&> (.city)
  let ghcCond :: DA.Object = DAKM.fromList [("city", show city)]
  let context' = context <> ghcCond
  let tenant' = maybe "" Data.Text.unpack tenant
  let toss' = fromMaybe 2 toss
  contextValue <- getConfigFromCacAsString context' tenant' toss'
  case contextValue of
    Nothing -> do
      logError $ "Error in getting frontend configs for City: " <> show city <> "toss: " <> show toss'
      return Nothing
    Just cfgs -> return $ Just cfgs
