{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | A dashboard block requested while the driver is on a ride is not applied immediately: the
--   driver must finish the ride first. The request is parked here, in Redis, until the driver
--   comes off the ride (ride end or cancel), at which point it is applied as the dashboard block
--   it would have been. While it is parked the driver is kept out of forward-batching pools so
--   that no further ride is attached to the one in progress.
--
--   Only the Redis side lives here, with minimal imports, so that the pool code under
--   Storage.Queries can read the key without a module cycle. The block itself is applied by
--   SharedLogic.Person.applyPendingOnRideBlock.
module SharedLogic.PendingOnRideBlock
  ( PendingOnRideBlock (..),
    pendingOnRideBlockKey,
    setPendingOnRideBlock,
    getPendingOnRideBlock,
    hasPendingOnRideBlock,
    clearPendingOnRideBlock,
  )
where

import qualified Domain.Types.Person as DP
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common

-- | The dashboard block request as it was made, so that it can be applied unchanged later.
data PendingOnRideBlock = PendingOnRideBlock
  { reasonCode :: Text,
    blockReason :: Maybe Text,
    blockTimeInHours :: Maybe Int,
    dashboardUserName :: Text,
    requestedAt :: UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON)

pendingOnRideBlockKey :: Id DP.Person -> Text
pendingOnRideBlockKey driverId = "driver-offer:PendingOnRideBlock:" <> driverId.getId

-- | Safety net only: the key is cleared when the block is applied or the driver is unblocked. A
--   ride that never reaches either should not pin the key forever.
pendingOnRideBlockTtl :: Int
pendingOnRideBlockTtl = 30 * 24 * 60 * 60

setPendingOnRideBlock :: CacheFlow m r => Id DP.Person -> PendingOnRideBlock -> m ()
setPendingOnRideBlock driverId pending = Hedis.setExp (pendingOnRideBlockKey driverId) pending pendingOnRideBlockTtl

getPendingOnRideBlock :: CacheFlow m r => Id DP.Person -> m (Maybe PendingOnRideBlock)
getPendingOnRideBlock driverId = Hedis.safeGet (pendingOnRideBlockKey driverId)

hasPendingOnRideBlock :: CacheFlow m r => Id DP.Person -> m Bool
hasPendingOnRideBlock driverId = isJust <$> getPendingOnRideBlock driverId

clearPendingOnRideBlock :: CacheFlow m r => Id DP.Person -> m ()
clearPendingOnRideBlock driverId = Hedis.del (pendingOnRideBlockKey driverId)
