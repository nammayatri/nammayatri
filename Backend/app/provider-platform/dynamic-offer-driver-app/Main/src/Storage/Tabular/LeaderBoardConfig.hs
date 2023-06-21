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

module Storage.Tabular.LeaderBoardConfig where

import qualified Domain.Types.LeaderBoardConfig as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Common hiding (id)
import Kernel.Types.Id
import Storage.Tabular.Merchant (MerchantTId)

derivePersistField "Domain.LeaderBoardType"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    LeaderBoardConfigsT sql=leader_board_configs
      id Text
      leaderBoardType Domain.LeaderBoardType
      numberOfSets Int
      leaderBoardExpiry Seconds
      zScoreBase Int
      leaderBoardLengthLimit Int
      isEnabled Bool
      merchantId MerchantTId
      Primary id
      deriving Generic
    |]

instance TEntityKey LeaderBoardConfigsT where
  type DomainKey LeaderBoardConfigsT = Id Domain.LeaderBoardConfigs
  fromKey (LeaderBoardConfigsTKey _id) = Id _id
  toKey (Id id) = LeaderBoardConfigsTKey id

instance FromTType LeaderBoardConfigsT Domain.LeaderBoardConfigs where
  fromTType LeaderBoardConfigsT {..} = do
    return $
      Domain.LeaderBoardConfigs
        { id = Id id,
          leaderBoardLengthLimit = fromIntegral leaderBoardLengthLimit,
          merchantId = fromKey merchantId,
          ..
        }

instance ToTType LeaderBoardConfigsT Domain.LeaderBoardConfigs where
  toTType Domain.LeaderBoardConfigs {..} =
    LeaderBoardConfigsT
      { id = getId id,
        leaderBoardLengthLimit = fromIntegral leaderBoardLengthLimit,
        merchantId = toKey merchantId,
        ..
      }
