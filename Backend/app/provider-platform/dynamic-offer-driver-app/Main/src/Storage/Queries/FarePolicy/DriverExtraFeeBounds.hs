{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TypeApplications #-}

module Storage.Queries.FarePolicy.DriverExtraFeeBounds where

import qualified Domain.Types.FarePolicy as DFP
-- import qualified Domain.Types.FarePolicy as FarePolicy
import qualified EulerHS.KVConnector.Flow as KV
import EulerHS.KVConnector.Types
import qualified EulerHS.Language as L
import qualified Kernel.Beam.Types as KBT
import Kernel.Prelude
-- import Kernel.Storage.Esqueleto as Esq

import Kernel.Types.Common
import Kernel.Types.Id as KTI
import Lib.Utils
import qualified Sequelize as Se
import qualified Storage.Beam.FarePolicy.DriverExtraFeeBounds as BeamDEFB
import qualified Storage.Tabular.FarePolicy.DriverExtraFeeBounds as Domain

-- create :: Domain.FullDriverExtraFeeBounds -> SqlDB ()
-- create = Esq.create

create :: L.MonadFlow m => Domain.FullDriverExtraFeeBounds -> m (MeshResult ())
create fullDriverExtraFeeBounds = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamDEFB.DriverExtraFeeBoundsT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' -> KV.createWoReturingKVConnector dbConf' updatedMeshConfig (transformDomainDriverExtraFeeBoundsToBeam fullDriverExtraFeeBounds)
    Nothing -> pure (Left $ MKeyNotFound "DB Config not found")

-- findByFarePolicyIdAndStartDistance :: Transactionable m => Id DFP.FarePolicy -> Meters -> m (Maybe Domain.FullDriverExtraFeeBounds)
-- findByFarePolicyIdAndStartDistance farePolicyId startDistance = Esq.findOne $ do
--   farePolicy <- from $ table @DFP.DriverExtraFeeBoundsT
--   where_ $
--     farePolicy ^. Domain.DriverExtraFeeBoundsFarePolicyId ==. val (toKey farePolicyId)
--       &&. farePolicy ^. Domain.DriverExtraFeeBoundsStartDistance ==. val startDistance
--   pure farePolicy

findByFarePolicyIdAndStartDistance :: L.MonadFlow m => Id DFP.FarePolicy -> Meters -> m (Maybe Domain.FullDriverExtraFeeBounds)
findByFarePolicyIdAndStartDistance (Id farePolicyId) startDistance = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamDEFB.DriverExtraFeeBoundsT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' ->
      either (pure Nothing) (transformBeamDriverExtraFeeBoundsToDomain <$>)
        <$> KV.findWithKVConnector dbConf' updatedMeshConfig [Se.And [Se.Is BeamDEFB.farePolicyId $ Se.Eq farePolicyId, Se.Is BeamDEFB.startDistance $ Se.Eq startDistance]]
    Nothing -> pure Nothing

-- update :: Id DFP.FarePolicy -> Meters -> Money -> Money -> SqlDB ()
-- update farePolicyId startDistace minFee maxFee = do
--   Esq.update $ \tbl -> do
--     set
--       tbl
--       [ Domain.DriverExtraFeeBoundsMinFee =. val minFee,
--         Domain.DriverExtraFeeBoundsMaxFee =. val maxFee
--       ]
--     where_ $
--       tbl ^. Domain.DriverExtraFeeBoundsFarePolicyId ==. val (toKey farePolicyId)
--         &&. tbl ^. Domain.DriverExtraFeeBoundsStartDistance ==. val startDistace

update :: L.MonadFlow m => Id DFP.FarePolicy -> Meters -> Money -> Money -> m ()
update (Id farePolicyId) startDistance minFee maxFee = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamDEFB.DriverExtraFeeBoundsT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' ->
      void $
        KV.updateWoReturningWithKVConnector
          dbConf'
          updatedMeshConfig
          [ Se.Set BeamDEFB.minFee minFee,
            Se.Set BeamDEFB.maxFee maxFee
          ]
          [Se.And [Se.Is BeamDEFB.farePolicyId $ Se.Eq farePolicyId, Se.Is BeamDEFB.startDistance $ Se.Eq startDistance]]
    Nothing -> pure ()

findAll ::
  ( L.MonadFlow m
  -- , Log m
  ) =>
  Id DFP.FarePolicy ->
  m [Domain.FullDriverExtraFeeBounds]
findAll farePolicyId = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamDEFB.DriverExtraFeeBoundsT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbCOnf' -> either (pure []) (transformBeamDriverExtraFeeBoundsToDomain <$>) <$> KV.findAllWithOptionsKVConnector dbCOnf' updatedMeshConfig [Se.Is BeamDEFB.farePolicyId $ Se.Eq (getId farePolicyId)] (Se.Asc BeamDEFB.startDistance) Nothing Nothing
    Nothing -> pure []

transformBeamDriverExtraFeeBoundsToDomain :: BeamDEFB.DriverExtraFeeBounds -> Domain.FullDriverExtraFeeBounds
transformBeamDriverExtraFeeBoundsToDomain BeamDEFB.DriverExtraFeeBoundsT {..} = do
  ( KTI.Id farePolicyId,
    DFP.DriverExtraFeeBounds
      { startDistance = startDistance,
        minFee = minFee,
        maxFee = maxFee
      }
    )

transformDomainDriverExtraFeeBoundsToBeam :: Domain.FullDriverExtraFeeBounds -> BeamDEFB.DriverExtraFeeBounds
transformDomainDriverExtraFeeBoundsToBeam (KTI.Id farePolicyId, DFP.DriverExtraFeeBounds {..}) =
  BeamDEFB.DriverExtraFeeBoundsT
    { id = Nothing,
      farePolicyId = farePolicyId,
      startDistance = startDistance,
      minFee = minFee,
      maxFee = maxFee
    }
