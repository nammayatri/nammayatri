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
import qualified EulerHS.KVConnector.Flow as KV
import qualified EulerHS.Language as L
import qualified Kernel.Beam.Types as KBT
import Kernel.Prelude
import Kernel.Types.Id
import Lib.Utils (setMeshConfig)
import qualified Sequelize as Se
import qualified Storage.Beam.LeaderBoardConfig as BeamLBC

-- findLeaderBoardConfigbyType :: (Transactionable m) => LeaderBoardType -> Id Merchant -> m (Maybe LeaderBoardConfigs)
-- findLeaderBoardConfigbyType leaderBType merchantId = Esq.findOne $ do
--   leaderBoardConfig <- from $ table @LeaderBoardConfigsT
--   where_ $
--     leaderBoardConfig ^. LeaderBoardConfigsLeaderBoardType ==. val leaderBType
--       &&. leaderBoardConfig ^. LeaderBoardConfigsMerchantId ==. val (toKey merchantId)

--   pure leaderBoardConfig

findLeaderBoardConfigbyType :: (L.MonadFlow m) => LeaderBoardType -> Id Merchant -> m (Maybe LeaderBoardConfigs)
findLeaderBoardConfigbyType leaderBType (Id merchantId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamLBC.LeaderBoardConfigsT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' ->
      either (pure Nothing) (transformBeamLeaderBoardConfigToDomain <$>)
        <$> KV.findWithKVConnector
          dbConf'
          updatedMeshConfig
          [ Se.And
              [ Se.Is BeamLBC.leaderBoardType $ Se.Eq leaderBType,
                Se.Is BeamLBC.merchantId $ Se.Eq merchantId
              ]
          ]
    Nothing -> pure Nothing

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
