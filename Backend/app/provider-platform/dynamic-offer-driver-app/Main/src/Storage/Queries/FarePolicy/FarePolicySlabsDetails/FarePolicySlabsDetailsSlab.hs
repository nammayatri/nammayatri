{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.FarePolicy.FarePolicySlabsDetails.FarePolicySlabsDetailsSlab where

import qualified Domain.Types.FarePolicy as DFP
import qualified EulerHS.KVConnector.Flow as KV
import qualified EulerHS.Language as L
import qualified Kernel.Beam.Types as KBT
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import qualified Kernel.Types.Id as KTI
import Kernel.Utils.Common
import Lib.Utils (setMeshConfig)
import qualified Sequelize as Se
import qualified Storage.Beam.FarePolicy.FarePolicySlabDetails.FarePolicySlabDetailsSlab as BeamFPSS
import Storage.Tabular.FarePolicy.FarePolicySlabsDetails.FarePolicySlabsDetailsSlab

findAll' ::
  ( Transactionable m,
    Monad m,
    MonadThrow m,
    Log m
  ) =>
  Id DFP.FarePolicy ->
  DTypeBuilder m [FarePolicySlabsDetailsSlabT]
findAll' farePolicyId = do
  Esq.findAll' $ do
    farePolicySlabsDetailsSlab <- from $ table @FarePolicySlabsDetailsSlabT
    where_ $
      farePolicySlabsDetailsSlab ^. FarePolicySlabsDetailsSlabFarePolicyId ==. val (toKey farePolicyId)
    orderBy [asc $ farePolicySlabsDetailsSlab ^. FarePolicySlabsDetailsSlabStartDistance]
    return farePolicySlabsDetailsSlab

findAll'' ::
  L.MonadFlow m =>
  Id DFP.FarePolicy ->
  m [FullFarePolicySlabsDetailsSlab]
findAll'' (Id farePolicyId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamFPSS.FarePolicySlabsDetailsSlabT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbCOnf' -> either (pure []) (transformBeamFarePolicyProgressiveDetailsToDomain <$>) <$> KV.findAllWithKVConnector dbCOnf' updatedMeshConfig [Se.Is BeamFPSS.farePolicyId $ Se.Eq farePolicyId]
    Nothing -> pure []

findById'' ::
  (L.MonadFlow m) =>
  Id DFP.FarePolicy ->
  m (Maybe FullFarePolicySlabsDetailsSlab)
findById'' (Id farePolicyId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamFPSS.FarePolicySlabsDetailsSlabT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbCOnf' -> either (pure Nothing) (transformBeamFarePolicyProgressiveDetailsToDomain <$>) <$> KV.findWithKVConnector dbCOnf' updatedMeshConfig [Se.Is BeamFPSS.farePolicyId $ Se.Eq farePolicyId]
    Nothing -> pure Nothing

deleteAll' :: Id DFP.FarePolicy -> FullEntitySqlDB ()
deleteAll' farePolicyId =
  Esq.delete' $ do
    farePolicySlabsDetailsSlab <- from $ table @FarePolicySlabsDetailsSlabT
    where_ $
      farePolicySlabsDetailsSlab ^. FarePolicySlabsDetailsSlabFarePolicyId ==. val (toKey farePolicyId)

deleteAll'' :: L.MonadFlow m => Id DFP.FarePolicy -> m ()
deleteAll'' (Id farePolicyId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamFPSS.FarePolicySlabsDetailsSlabT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbCOnf' -> void $ KV.deleteAllReturningWithKVConnector dbCOnf' updatedMeshConfig [Se.Is BeamFPSS.farePolicyId $ Se.Eq farePolicyId]
    Nothing -> pure ()

transformBeamFarePolicyProgressiveDetailsToDomain :: BeamFPSS.FarePolicySlabsDetailsSlab -> FullFarePolicySlabsDetailsSlab
transformBeamFarePolicyProgressiveDetailsToDomain BeamFPSS.FarePolicySlabsDetailsSlabT {..} = do
  ( KTI.Id farePolicyId,
    DFP.FPSlabsDetailsSlab
      { startDistance = startDistance,
        baseFare = baseFare,
        waitingChargeInfo =
          ((,) <$> waitingCharge <*> freeWatingTime) <&> \(waitingCharge', freeWaitingTime') ->
            DFP.WaitingChargeInfo
              { waitingCharge = waitingCharge',
                freeWaitingTime = freeWaitingTime'
              },
        nightShiftCharge = nightShiftCharge
      }
    )

transformDomainFarePolicySlabsDetailsSlabToBeam :: FullFarePolicySlabsDetailsSlab -> BeamFPSS.FarePolicySlabsDetailsSlab
transformDomainFarePolicySlabsDetailsSlabToBeam (KTI.Id farePolicyId, DFP.FPSlabsDetailsSlab {..}) =
  BeamFPSS.FarePolicySlabsDetailsSlabT
    { id = Nothing,
      farePolicyId = farePolicyId,
      startDistance = startDistance,
      baseFare = baseFare,
      waitingCharge = DFP.waitingCharge <$> waitingChargeInfo,
      nightShiftCharge = nightShiftCharge,
      freeWatingTime = DFP.freeWaitingTime <$> waitingChargeInfo
    }
