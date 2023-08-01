{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.FareParameters where

import Domain.Types.FareParameters as DFP
import qualified EulerHS.Language as L
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Types.Logging (Log)
import Lib.Utils (FromTType' (fromTType'), ToTType' (toTType'), createWithKV, findAllWithKV, findAllWithKvInReplica, findOneWithKV, findOneWithKvInReplica)
import qualified Sequelize as Se
import qualified Storage.Beam.FareParameters as BeamFP
import Storage.Queries.FareParameters.FareParametersProgressiveDetails as QFPPD
import qualified Storage.Queries.FareParameters.FareParametersProgressiveDetails as BeamFPPD
import Storage.Queries.FareParameters.FareParametersSlabDetails as QFPSD
import qualified Storage.Queries.FareParameters.FareParametersSlabDetails as BeamFPSD

create :: (L.MonadFlow m, Log m) => DFP.FareParameters -> m ()
create fareParameters = do
  createWithKV fareParameters
  case fareParameters.fareParametersDetails of
    ProgressiveDetails fppdt -> QFPPD.create (fareParameters.id, fppdt)
    SlabDetails fpsdt -> QFPSD.create (fareParameters.id, fpsdt)

findById :: (L.MonadFlow m, Log m) => Id FareParameters -> m (Maybe FareParameters)
findById (Id fareParametersId) = findOneWithKV [Se.Is BeamFP.id $ Se.Eq fareParametersId]

findByIdInReplica :: (L.MonadFlow m, Log m) => Id FareParameters -> m (Maybe FareParameters)
findByIdInReplica (Id fareParametersId) = findOneWithKvInReplica [Se.Is BeamFP.id $ Se.Eq fareParametersId]

findAllIn :: (L.MonadFlow m, Log m) => [Id FareParameters] -> m [FareParameters]
findAllIn fareParametersIds = findAllWithKV [Se.Is BeamFP.id $ Se.In $ getId <$> fareParametersIds]

findAllInInReplica :: (L.MonadFlow m, Log m) => [Id FareParameters] -> m [FareParameters]
findAllInInReplica fareParametersIds = findAllWithKvInReplica [Se.Is BeamFP.id $ Se.In $ getId <$> fareParametersIds]

-- findAllIn :: Transactionable m => [Id FareParameters] -> m [FareParameters]
-- findAllIn fareParamIds =
--   buildDType $ do
--     res <- Esq.findAll' $ do
--       fareParamFile <- from $ table @FareParametersT
--       pure fareParamFile
--     catMaybes <$> mapM buildFullFareParameters res

instance FromTType' BeamFP.FareParameters FareParameters where
  fromTType' BeamFP.FareParametersT {..} = do
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

instance ToTType' BeamFP.FareParameters FareParameters where
  toTType' FareParameters {..} = do
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

findAllLateNightRides :: Transactionable m => [Id FareParameters] -> m Int
findAllLateNightRides fareParamIds =
  mkCount <$> do
    Esq.findAll $ do
      fareParamFile <- from $ table @FareParametersT
      where_ $
        fareParamFile ^. FareParametersId `in_` valList (map getId fareParamIds)
          &&. not_ (Esq.isNothing (fareParamFile ^. FareParametersNightShiftCharge))
      return (countRows :: SqlExpr (Esq.Value Int))
  where
    mkCount [counter] = counter
    mkCount _ = 0

findDriverSelectedFareEarnings :: Transactionable m => [Id FareParameters] -> m Money
findDriverSelectedFareEarnings fareParamIds =
  mkSum
    <$> Esq.findAll do
      fareParamFile <- from $ table @FareParametersT
      where_ $ fareParamFile ^. FareParametersId `in_` valList (map getId fareParamIds)
      pure (sum_ $ fareParamFile ^. FareParametersDriverSelectedFare :: SqlExpr (Esq.Value (Maybe Money)))
  where
    mkSum [Just value] = value
    mkSum _ = 0

findCustomerExtraFees :: Transactionable m => [Id FareParameters] -> m Money
findCustomerExtraFees fareParamIds =
  mkSum
    <$> Esq.findAll do
      fareParamFile <- from $ table @FareParametersT
      where_ $ fareParamFile ^. FareParametersId `in_` valList (map getId fareParamIds)
      pure (sum_ $ fareParamFile ^. FareParametersCustomerExtraFee :: SqlExpr (Esq.Value (Maybe Money)))
  where
    mkSum [Just value] = value
    mkSum _ = 0
