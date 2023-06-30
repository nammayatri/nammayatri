{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.LeaderBoardConfig where

import Domain.Types.LeaderBoardConfig
import Domain.Types.Merchant
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import qualified Storage.Beam.LeaderBoardConfig as BeamLBC
import Storage.Tabular.LeaderBoardConfig

findLeaderBoardConfigbyType :: (Transactionable m) => LeaderBoardType -> Id Merchant -> m (Maybe LeaderBoardConfigs)
findLeaderBoardConfigbyType leaderBType merchantId = Esq.findOne $ do
  leaderBoardConfig <- from $ table @LeaderBoardConfigsT
  where_ $
    leaderBoardConfig ^. LeaderBoardConfigsLeaderBoardType ==. val leaderBType
      &&. leaderBoardConfig ^. LeaderBoardConfigsMerchantId ==. val (toKey merchantId)

  pure leaderBoardConfig

transformBeamLeaderBoardConfigToDomain :: BeamLBC.LeaderBoardConfigs -> LeaderBoardConfigs
transformBeamLeaderBoardConfigToDomain BeamLBC.LeaderBoardConfigsT {..} = do
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

transformDomainLeaderBoardConfigToBeam :: LeaderBoardConfigs -> BeamLBC.LeaderBoardConfigs
transformDomainLeaderBoardConfigToBeam LeaderBoardConfigs {..} =
  BeamLBC.defaultLeaderBoardConfig
    { BeamLBC.id = getId id,
      BeamLBC.leaderBoardType = leaderBoardType,
      BeamLBC.numberOfSets = numberOfSets,
      BeamLBC.leaderBoardExpiry = leaderBoardExpiry,
      BeamLBC.zScoreBase = zScoreBase,
      BeamLBC.leaderBoardLengthLimit = fromIntegral leaderBoardLengthLimit,
      BeamLBC.isEnabled = isEnabled,
      BeamLBC.merchantId = getId merchantId
    }
