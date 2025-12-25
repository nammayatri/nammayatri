{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.ValueAddNP
  ( isValueAddNP,
  )
where

import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Utils.Common
import qualified Storage.Queries.ValueAddNP as Queries

isValueAddNP :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => Text -> m Bool
isValueAddNP subscriberId =
  Hedis.safeGet lookupKey >>= \case
    Just subscriberIds -> return $ checkIfValueAddNP subscriberId subscriberIds
    Nothing -> do
      subscriberIds <- Queries.findAll True <&> map (.subscriberId)
      cacheValueAddNPList subscriberIds
      return $ checkIfValueAddNP subscriberId subscriberIds

checkIfValueAddNP :: Text -> [Text] -> Bool
checkIfValueAddNP subscriberId subscriberIds = subscriberId `elem` subscriberIds

lookupKey :: Text
lookupKey = "CachedQueries:ValueAddNP"

cacheValueAddNPList :: (CacheFlow m r) => [Text] -> m ()
cacheValueAddNPList subscriberIds = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.setExp lookupKey subscriberIds expTime
