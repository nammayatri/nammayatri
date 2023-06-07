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
import qualified EulerHS.KVConnector.Flow as KV
import qualified EulerHS.Language as L
import qualified Kernel.Beam.Types as KBT
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id as KTI
import Kernel.Utils.Common
import qualified Lib.Mesh as Mesh
import qualified Sequelize as Se
import qualified Storage.Beam.FarePolicy.DriverExtraFeeBounds as BeamDEFB
import Storage.Tabular.FarePolicy.DriverExtraFeeBounds
import qualified Storage.Tabular.FarePolicy.DriverExtraFeeBounds as Domain

findAll' ::
  ( Transactionable m,
    Monad m,
    MonadThrow m,
    Log m
  ) =>
  Id DFP.FarePolicy ->
  DTypeBuilder m [DriverExtraFeeBoundsT]
findAll' farePolicyId = do
  Esq.findAll' $ do
    driverExtraFeeBounds <- from $ table @DriverExtraFeeBoundsT
    where_ $
      driverExtraFeeBounds ^. DriverExtraFeeBoundsFarePolicyId ==. val (toKey farePolicyId)
    orderBy [asc $ driverExtraFeeBounds ^. DriverExtraFeeBoundsStartDistance]
    return driverExtraFeeBounds

findAll ::
  ( L.MonadFlow m
  -- , Log m
  ) =>
  Id DFP.FarePolicy ->
  m [Domain.FullDriverExtraFeeBounds]
findAll farePolicyId = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  case dbConf of
    Just dbCOnf' -> either (pure []) (transformBeamDriverExtraFeeBoundsToDomain <$>) <$> KV.findAllWithOptionsKVConnector dbCOnf' Mesh.meshConfig [Se.Is BeamDEFB.farePolicyId $ Se.Eq (getId farePolicyId)] (Se.Asc BeamDEFB.startDistance) Nothing Nothing
    Nothing -> pure []

deleteAll' :: Id DFP.FarePolicy -> FullEntitySqlDB ()
deleteAll' farePolicyId =
  Esq.delete' $ do
    driverExtraFeeBounds <- from $ table @DriverExtraFeeBoundsT
    where_ $
      driverExtraFeeBounds ^. DriverExtraFeeBoundsFarePolicyId ==. val (toKey farePolicyId)

transformBeamDriverExtraFeeBoundsToDomain :: BeamDEFB.DriverExtraFeeBounds -> Domain.FullDriverExtraFeeBounds
transformBeamDriverExtraFeeBoundsToDomain BeamDEFB.DriverExtraFeeBoundsT {..} = do
  ( KTI.Id farePolicyId,
    DFP.DriverExtraFeeBounds
      { -- id = id,
        -- farePolicyId = getId farePolicyId,
        startDistance = startDistance,
        minFee = minFee,
        maxFee = maxFee
      }
    )

transformDomainDriverExtraFeeBoundsToBeam :: Domain.FullDriverExtraFeeBounds -> BeamDEFB.DriverExtraFeeBounds
transformDomainDriverExtraFeeBoundsToBeam (KTI.Id farePolicyId, DFP.DriverExtraFeeBounds {..}) =
  BeamDEFB.DriverExtraFeeBoundsT
    { -- id = id,
      farePolicyId = farePolicyId,
      startDistance = startDistance,
      minFee = minFee,
      maxFee = maxFee
    }
