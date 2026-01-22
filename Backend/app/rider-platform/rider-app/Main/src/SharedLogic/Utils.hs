{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Utils where

import qualified Data.Time as Time
import qualified Domain.Types.Person as DP
import EulerHS.Prelude
import Kernel.Storage.Esqueleto.Config
import Kernel.Utils.Common
import qualified Kernel.Utils.UUID as UUID
import qualified Storage.CachedQueries.Merchant.RiderConfig as QRC

getStaticCustomerId :: (MonadFlow m, EsqDBReplicaFlow m r, EsqDBFlow m r, CacheFlow m r) => DP.Person -> Text -> m Text
getStaticCustomerId person phone = do
  mbRiderConfig <- QRC.findByMerchantOperatingCityId person.merchantOperatingCityId Nothing
  let mbThreshold = mbRiderConfig >>= (.staticCustomerIdThresholdDay)
  case mbThreshold of
    Just threshold ->
      if Time.utctDay person.createdAt > threshold
        then do
          let key = phone <> ":" <> person.merchantId.getId
          return $ UUID.generateStaticUUID key
        else return person.id.getId
    Nothing -> return person.id.getId
