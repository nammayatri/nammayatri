{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.Merchant.LeaderBoardConfig where

import Domain.Types.Merchant.LeaderBoardConfig
import Domain.Types.Merchant.MerchantOperatingCity
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.Merchant.LeaderBoardConfig as BeamLBC

findLeaderBoardConfigbyType :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => LeaderBoardType -> Id MerchantOperatingCity -> m (Maybe LeaderBoardConfigs)
findLeaderBoardConfigbyType leaderBType merchantOperatingCityId = findOneWithKV [Se.And [Se.Is BeamLBC.leaderBoardType $ Se.Eq leaderBType, Se.Is BeamLBC.merchantOperatingCityId $ Se.Eq (getId merchantOperatingCityId)]]

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
            merchantId = Id merchantId,
            merchantOperatingCityId = Id merchantOperatingCityId
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
        BeamLBC.merchantId = getId merchantId,
        BeamLBC.merchantOperatingCityId = getId merchantOperatingCityId
      }
