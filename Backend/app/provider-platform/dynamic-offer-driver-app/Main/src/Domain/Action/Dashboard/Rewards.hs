{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Dashboard.Rewards where
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess (Success))
import qualified Storage.Queries.Rewards as QRD
import qualified Domain.Types.Rewards as D
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Utils.Common
data RewardReq = RewardReq
  { name :: Text,
    provider :: Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)


createReward ::
  ( MonadFlow m,
    EsqDBFlow m r
  ) =>
  RewardReq ->
  m APISuccess
createReward RewardReq {..} = do
  rewardId <- generateGUID
  createRewardRecord <- mkcreateRewardType provider name rewardId
  Esq.runTransaction $ QRD.create createRewardRecord
  pure Success
  where
    mkcreateRewardType pv nm rewardId = do
      now <- getCurrentTime
      pure $
        D.Reward
          { id = rewardId,
            name = nm,
            provider = pv,
            createdAt = now,
            updatedAt = now
          }
