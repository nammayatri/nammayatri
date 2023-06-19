{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.FareParameters where

import Domain.Types.FareParameters as DFP
import qualified EulerHS.KVConnector.Flow as KV
import qualified EulerHS.Language as L
import qualified Kernel.Beam.Types as KBT
import Kernel.Prelude
import Kernel.Types.Id
import Lib.Utils (setMeshConfig)
import qualified Sequelize as Se
import qualified Storage.Beam.FareParameters as BeamFP
import qualified Storage.Queries.FareParameters.FareParametersProgressiveDetails as BeamFPPD
import qualified Storage.Queries.FareParameters.FareParametersSlabDetails as BeamFPSD

create :: L.MonadFlow m => DFP.FareParameters -> m ()
create fareParameters = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamFP.FareParametersT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' -> do
      void $ KV.createWoReturingKVConnector dbConf' updatedMeshConfig (transformDomainFareParametersToBeam fareParameters)
      case fareParameters.fareParametersDetails of
        ProgressiveDetails fppdt -> do
          void $ KV.createWoReturingKVConnector dbConf' updatedMeshConfig (BeamFPPD.transformDomainFareParametersProgressiveDetailsToBeam (fareParameters.id, fppdt))
        SlabDetails fpsdt -> do
          void $ KV.createWoReturingKVConnector dbConf' updatedMeshConfig (BeamFPSD.transformDomainFareParametersSlabDetailsToBeam (fareParameters.id, fpsdt))
    Nothing -> pure ()

findById :: L.MonadFlow m => Id FareParameters -> m (Maybe FareParameters)
findById (Id fareParametersId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamFP.FareParametersT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbCOnf' -> do
      fp <- KV.findWithKVConnector dbCOnf' updatedMeshConfig [Se.Is BeamFP.id $ Se.Eq fareParametersId]
      case fp of
        Right (Just fp') -> transformBeamFareParametersToDomain fp'
        _ -> pure Nothing
    -- either (pure Nothing) (transformBeamFareParametersToDomain <$>) <$> KV.findWithKVConnector dbCOnf' updatedMeshConfig [Se.Is BeamFP.id $ Se.Eq fareParametersId]
    Nothing -> pure Nothing

-- TODO @Vijay Gupta, Change the following query. Done
-- create :: FareParameters -> SqlDB ()
-- create fareParams =
--   withFullEntity fareParams $ \(fareParams', fareParamsDetais) -> do
--     Esq.create' fareParams'
--     case fareParamsDetais of
--       ProgressiveDetailsT fppdt -> Esq.create' fppdt
--       SlabDetailsT fpsdt -> Esq.create' fpsdt

transformBeamFareParametersToDomain :: L.MonadFlow m => BeamFP.FareParameters -> m (Maybe FareParameters)
transformBeamFareParametersToDomain BeamFP.FareParametersT {..} = do
  fullFPPD <- BeamFPPD.findById' (Id id)
  let fPPD = snd $ fromJust fullFPPD
  fullFPSD <- BeamFPSD.findById' (Id id)
  let fPSD = snd $ fromJust fullFPSD
  if isJust fullFPPD
    then
      pure $
        Just
          FareParameters
            { id = Id id,
              driverSelectedFare = driverSelectedFare,
              customerExtraFee = customerExtraFee,
              serviceCharge = serviceCharge,
              nightShiftRateIfApplies = nightShiftRateIfApplies,
              govtCharges = govtCharges,
              baseFare = baseFare,
              waitingCharge = waitingCharge,
              nightShiftCharge = nightShiftCharge,
              fareParametersDetails = case fareParametersType of
                Progressive -> ProgressiveDetails fPPD
                Slab -> SlabDetails fPSD
            }
    else pure Nothing

transformDomainFareParametersToBeam :: FareParameters -> BeamFP.FareParameters
transformDomainFareParametersToBeam FareParameters {..} =
  BeamFP.FareParametersT
    { BeamFP.id = getId id,
      BeamFP.driverSelectedFare = driverSelectedFare,
      BeamFP.customerExtraFee = customerExtraFee,
      BeamFP.serviceCharge = serviceCharge,
      BeamFP.govtCharges = govtCharges,
      BeamFP.nightShiftRateIfApplies = nightShiftRateIfApplies,
      BeamFP.baseFare = baseFare,
      BeamFP.waitingCharge = waitingCharge,
      BeamFP.nightShiftCharge = nightShiftCharge,
      BeamFP.fareParametersType = getFareParametersType $ FareParameters {..}
    }

findAllIn :: Transactionable m => [Id FareParameters] -> m [FareParameters]
findAllIn fareParamIds =
  buildDType $ do
    res <- Esq.findAll' $ do
      fareParamFile <- from $ table @FareParametersT
      where_ $
        fareParamFile ^. FareParametersId `in_` valList (map getId fareParamIds)
      pure fareParamFile
    catMaybes <$> mapM buildFullFareParameters res
