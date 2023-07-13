{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.BlackListOrg
  {-# WARNING
    "This module contains direct calls to the table. \
  \ But most likely you need a version from CachedQueries with caching results feature."
    #-}
where

import Domain.Types.BlackListOrg
import qualified EulerHS.KVConnector.Flow as KV
import qualified EulerHS.Language as L
import qualified Kernel.Beam.Types as KBT
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Types.Registry.Subscriber (Subscriber)
import Lib.Utils
import qualified Sequelize as Se
import qualified Storage.Beam.BlackListOrg as BeamBLO

-- findBySubscriberId :: Transactionable m => ShortId Subscriber -> m (Maybe BlackListOrg)
-- findBySubscriberId subscriberId = do
--   findOne $ do
--     org <- from $ table @BlackListOrgT
--     where_ $ org ^. BlackListOrgSubscriberId ==. val (getShortId subscriberId)
--     return org

findBySubscriberId :: L.MonadFlow m => ShortId Subscriber -> m (Maybe BlackListOrg)
findBySubscriberId subscriberId = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamBLO.BlackListOrgT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' -> either (pure Nothing) (transformBeamBlackListOrgToDomain <$>) <$> KV.findWithKVConnector dbConf' updatedMeshConfig [Se.Is BeamBLO.subscriberId $ Se.Eq $ getShortId subscriberId]
    Nothing -> pure Nothing

transformBeamBlackListOrgToDomain :: BeamBLO.BlackListOrg -> BlackListOrg
transformBeamBlackListOrgToDomain BeamBLO.BlackListOrgT {..} = do
  BlackListOrg
    { id = Id id,
      subscriberId = ShortId subscriberId,
      _type = orgType
    }

transformDomainBlackListOrgToBeam :: BlackListOrg -> BeamBLO.BlackListOrg
transformDomainBlackListOrgToBeam BlackListOrg {..} =
  BeamBLO.BlackListOrgT
    { BeamBLO.id = getId id,
      BeamBLO.subscriberId = getShortId subscriberId,
      BeamBLO.orgType = _type
    }
