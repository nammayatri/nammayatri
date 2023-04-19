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

module Storage.Tabular.Reward where

import qualified Domain.Types.Reward as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id

derivePersistField "Domain.Units"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    RewardT sql=reward
      id Text
      name Text
      provider Text
      quantity Int
      quantityUnit Domain.Units
      createdAt UTCTime
      updatedAt UTCTime

      Primary id
      deriving Generic
    |]

instance TEntityKey RewardT where
  type DomainKey RewardT = Id Domain.Reward
  fromKey (RewardTKey _id) = Id _id
  toKey (Id id) = RewardTKey id

instance FromTType RewardT Domain.Reward where
  fromTType RewardT {..} = do
    return $
      Domain.Reward
        { id = Id id,
          ..
        }

instance ToTType RewardT Domain.Reward where
  toTType Domain.Reward {..} =
    RewardT
      { id = getId id,
        ..
      }
