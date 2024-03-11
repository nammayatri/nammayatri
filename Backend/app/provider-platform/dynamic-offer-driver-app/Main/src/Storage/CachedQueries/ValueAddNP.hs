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

import Domain.Types.ValueAddNP
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Utils.Common
import qualified Storage.Queries.ValueAddNP as Queries

isValueAddNP :: KvDbFlow m r => Text -> m Bool
isValueAddNP subscriberId =
  Hedis.safeGet lookupKey >>= \case
    Just npList -> return $ checkIfValueAddNP subscriberId npList
    Nothing -> do
      npList <- Queries.findAll True
      cacheValueAddNPList npList
      return $ checkIfValueAddNP subscriberId npList

checkIfValueAddNP :: Text -> [ValueAddNP] -> Bool
checkIfValueAddNP subscriberId npList = subscriberId `elem` (map (.subscriberId) npList)

lookupKey :: Text
lookupKey = "CachedQueries:ValueAddNP"

cacheValueAddNPList :: (CacheFlow m r) => [ValueAddNP] -> m ()
cacheValueAddNPList npList = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let ids = map (.subscriberId) npList
  Hedis.setExp lookupKey ids expTime
