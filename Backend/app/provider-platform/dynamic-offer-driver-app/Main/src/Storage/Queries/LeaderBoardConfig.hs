{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.LeaderBoardConfig where

import Domain.Types.LeaderBoardConfig
import Domain.Types.Merchant
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.LeaderBoardConfig as BeamLBC

findLeaderBoardConfigbyType :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => LeaderBoardType -> Id Merchant -> m (Maybe LeaderBoardConfigs)
findLeaderBoardConfigbyType leaderBType (Id merchantId) =
  findOneWithKV
    [ Se.And
        [ Se.Is BeamLBC.leaderBoardType $ Se.Eq leaderBType,
          Se.Is BeamLBC.merchantId $ Se.Eq merchantId
        ]
    ]

instance FromTType' BeamLBC.LeaderBoardConfigs LeaderBoardConfigs where
  fromTType' BeamLBC.LeaderBoardConfigsT {..} = do
    pure $
      Just
        LeaderBoardConfigs
          { id = Id id,
            leaderBoardType = leaderBoardType,
            numberOfSets = numberOfSets,
            leaderBoardExpiry = leaderBoardExpiry,
            zScoreBase = zScoreBase,
            leaderBoardLengthLimit = fromIntegral leaderBoardLengthLimit,
            isEnabled = isEnabled,
            merchantId = Id merchantId
          }

instance ToTType' BeamLBC.LeaderBoardConfigs LeaderBoardConfigs where
  toTType' LeaderBoardConfigs {..} = do
    BeamLBC.LeaderBoardConfigsT
      { BeamLBC.id = getId id,
        BeamLBC.leaderBoardType = leaderBoardType,
        BeamLBC.numberOfSets = numberOfSets,
        BeamLBC.leaderBoardExpiry = leaderBoardExpiry,
        BeamLBC.zScoreBase = zScoreBase,
        BeamLBC.leaderBoardLengthLimit = fromIntegral leaderBoardLengthLimit,
        BeamLBC.isEnabled = isEnabled,
        BeamLBC.merchantId = getId merchantId
      }
