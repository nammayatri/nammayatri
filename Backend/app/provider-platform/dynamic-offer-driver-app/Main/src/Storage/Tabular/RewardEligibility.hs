{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.RewardEligibility where

import qualified Domain.Types.Rewards as Rewards
import qualified Domain.Types.RewardEligibility as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id
import Storage.Tabular.Person (PersonTId)


mkPersist
  defaultSqlSettings
  [defaultQQ|
    RewardEligibilityT sql=Reward_eligibility
      id Text
      rewardId Text
      driverId PersonTId
      quantity Int
      quantityUnit Rewards.Units
      collected Bool
      collectedAt UTCTime Maybe
      createdAt UTCTime
      updatedAt UTCTime
      Primary id
      deriving Generic
    |]
derivePersistField "Rewards.Units"

instance TEntityKey RewardEligibilityT where
  type DomainKey RewardEligibilityT = Id Domain.RewardEligibility
  fromKey (RewardEligibilityTKey _id) = Id _id
  toKey (Id id) = RewardEligibilityTKey id

instance FromTType RewardEligibilityT Domain.RewardEligibility where
  fromTType RewardEligibilityT {..} = do
    return $
      Domain.RewardEligibility
        { id = Id id,
          rewardId = Id rewardId,
          driverId = fromKey driverId,
          ..
        }

instance ToTType RewardEligibilityT Domain.RewardEligibility where
  toTType Domain.RewardEligibility {..} =
    RewardEligibilityT
      { id = getId id,
        rewardId = getId rewardId,
        driverId = toKey driverId,
        ..
      }
