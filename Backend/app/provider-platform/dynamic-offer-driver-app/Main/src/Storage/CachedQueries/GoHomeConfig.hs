{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.GoHomeConfig where

import Control.Monad
import Domain.Types.GoHomeConfig
import Domain.Types.Merchant
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.App (MonadFlow)
import Kernel.Types.CacheFlow (CacheFlow)
import Kernel.Types.Id (Id)
import Kernel.Utils.Error.Throwing
import qualified Storage.Queries.GoHomeConfig as Queries
import Tools.Error (GenericError (..))

findByMerchantId :: (CacheFlow m r, MonadFlow m) => Id Merchant -> m GoHomeConfig
findByMerchantId id = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.safeGet (makeGoHomeKey id) >>= \case
    Just cfg -> return cfg
    Nothing -> do
      cfg <- fromMaybeM (InternalError "Could not find Go-To config corresponding to the stated merchant id") =<< Queries.findByMerchantId id
      Hedis.setExp (makeGoHomeKey id) cfg expTime
      return cfg

makeGoHomeKey :: Id Merchant -> Text
makeGoHomeKey id = "driver-offer:CachedQueries:GoHomeConfig:MerchantId-" <> id.getId
