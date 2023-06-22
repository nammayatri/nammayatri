{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.FarePolicy.FarePolicyProgressiveDetails.FarePolicyProgressiveDetailsPerExtraKmRateSection where

-- import Data.Text (pack)
import qualified Domain.Types.FarePolicy as DFP
import qualified EulerHS.KVConnector.Flow as KV
import qualified EulerHS.Language as L
import qualified Kernel.Beam.Types as KBT
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id as KTI
import Kernel.Utils.Common
import Lib.Utils (setMeshConfig)
import Sequelize as Se
import qualified Storage.Beam.FarePolicy.FarePolicyProgressiveDetails.FarePolicyProgressiveDetailsPerExtraKmRateSection as BeamFPPDP
import Storage.Tabular.FarePolicy.FarePolicyProgressiveDetails.FarePolicyProgressiveDetailsPerExtraKmRateSection

findAll' ::
  ( Transactionable m,
    Monad m,
    MonadThrow m,
    Log m
  ) =>
  Id DFP.FarePolicy ->
  DTypeBuilder m [FarePolicyProgressiveDetailsPerExtraKmRateSectionT]
findAll' farePolicyId = do
  Esq.findAll' $ do
    farePolicyProgressiveDetailsPerExtraKmFareSection <- from $ table @FarePolicyProgressiveDetailsPerExtraKmRateSectionT
    where_ $
      farePolicyProgressiveDetailsPerExtraKmFareSection ^. FarePolicyProgressiveDetailsPerExtraKmRateSectionFarePolicyId ==. val (toKey farePolicyId)
    orderBy [asc $ farePolicyProgressiveDetailsPerExtraKmFareSection ^. FarePolicyProgressiveDetailsPerExtraKmRateSectionStartDistance]
    return farePolicyProgressiveDetailsPerExtraKmFareSection

findById' :: L.MonadFlow m => KTI.Id DFP.FarePolicy -> m (Maybe FullFarePolicyProgressiveDetailsPerExtraKmRateSection)
findById' farePolicyId' = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamFPPDP.FarePolicyProgressiveDetailsPerExtraKmRateSectionT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbCOnf' -> either (pure Nothing) (transformBeamFarePolicyProgressiveDetailsPerExtraKmRateSectionToDomain <$>) <$> KV.findWithKVConnector dbCOnf' updatedMeshConfig [Se.Is BeamFPPDP.farePolicyId $ Se.Eq (getId farePolicyId')]
    Nothing -> pure Nothing

findAll ::
  ( L.MonadFlow m
  ) =>
  -- Id DFP.FarePolicy ->
  Id DFP.FarePolicy ->
  m [FullFarePolicyProgressiveDetailsPerExtraKmRateSection]
findAll farePolicyId = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamFPPDP.FarePolicyProgressiveDetailsPerExtraKmRateSectionT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbCOnf' -> either (pure []) (transformBeamFarePolicyProgressiveDetailsPerExtraKmRateSectionToDomain <$>) <$> KV.findAllWithOptionsKVConnector dbCOnf' updatedMeshConfig [Se.Is BeamFPPDP.farePolicyId $ Se.Eq (getId farePolicyId)] (Se.Asc BeamFPPDP.startDistance) Nothing Nothing
    Nothing -> pure []

deleteAll' :: Id DFP.FarePolicy -> FullEntitySqlDB ()
deleteAll' farePolicyId =
  Esq.delete' $ do
    farePolicyProgressiveDetailsPerExtraKmFareSection <- from $ table @FarePolicyProgressiveDetailsPerExtraKmRateSectionT
    where_ $
      farePolicyProgressiveDetailsPerExtraKmFareSection ^. FarePolicyProgressiveDetailsPerExtraKmRateSectionFarePolicyId ==. val (toKey farePolicyId)

deleteAll'' :: L.MonadFlow m => Id DFP.FarePolicy -> m ()
deleteAll'' (Id farePolicyId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamFPPDP.FarePolicyProgressiveDetailsPerExtraKmRateSectionT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' ->
      void $
        KV.deleteWithKVConnector
          dbConf'
          updatedMeshConfig
          [Se.Is BeamFPPDP.farePolicyId $ Se.Eq farePolicyId]
    Nothing -> pure ()

transformBeamFarePolicyProgressiveDetailsPerExtraKmRateSectionToDomain :: BeamFPPDP.FarePolicyProgressiveDetailsPerExtraKmRateSection -> FullFarePolicyProgressiveDetailsPerExtraKmRateSection
transformBeamFarePolicyProgressiveDetailsPerExtraKmRateSectionToDomain BeamFPPDP.FarePolicyProgressiveDetailsPerExtraKmRateSectionT {..} = do
  ( KTI.Id farePolicyId,
    DFP.FPProgressiveDetailsPerExtraKmRateSection
      { startDistance = startDistance,
        perExtraKmRate = perExtraKmRate
      }
    )

transformDomainFarePolicyProgressiveDetailsPerExtraKmRateSectionToBeam :: FullFarePolicyProgressiveDetailsPerExtraKmRateSection -> BeamFPPDP.FarePolicyProgressiveDetailsPerExtraKmRateSection
transformDomainFarePolicyProgressiveDetailsPerExtraKmRateSectionToBeam (KTI.Id farePolicyId, DFP.FPProgressiveDetailsPerExtraKmRateSection {..}) =
  BeamFPPDP.FarePolicyProgressiveDetailsPerExtraKmRateSectionT
    { -- id = id,
      farePolicyId = farePolicyId,
      startDistance = startDistance,
      perExtraKmRate = perExtraKmRate
    }
