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
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.FareParameters as BeamFP
import Storage.Queries.FareParameters.FareParametersProgressiveDetails as QFPPD
import qualified Storage.Queries.FareParameters.FareParametersProgressiveDetails as BeamFPPD
import Storage.Queries.FareParameters.FareParametersSlabDetails as QFPSD
import qualified Storage.Queries.FareParameters.FareParametersSlabDetails as BeamFPSD

create :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => DFP.FareParameters -> m ()
create fareParameters = do
  createWithKV fareParameters
  case fareParameters.fareParametersDetails of
    ProgressiveDetails fppdt -> QFPPD.create (fareParameters.id, fppdt)
    SlabDetails fpsdt -> QFPSD.create (fareParameters.id, fpsdt)

findById :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id FareParameters -> m (Maybe FareParameters)
findById (Id fareParametersId) = findOneWithKV [Se.Is BeamFP.id $ Se.Eq fareParametersId]

findAllIn :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => [Id FareParameters] -> m [FareParameters]
findAllIn fareParametersIds = findAllWithKV [Se.Is BeamFP.id $ Se.In $ getId <$> fareParametersIds]

instance FromTType' BeamFP.FareParameters FareParameters where
  fromTType' BeamFP.FareParametersT {..} = do
    mFareParametersDetails <-
      case fareParametersType of
        Progressive -> do
          mFullFPPD <- BeamFPPD.findById' (Id id)
          case mFullFPPD of
            Just (_, fPPD) -> return (Just $ ProgressiveDetails fPPD)
            Nothing -> return Nothing
        Slab -> do
          mFullFPSD <- BeamFPSD.findById' (Id id)
          case mFullFPSD of
            Just (_, fPSD) -> return (Just $ SlabDetails fPSD)
            Nothing -> return Nothing
    case mFareParametersDetails of
      Just fareParametersDetails -> do
        return $
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
                rideExtraTimeFare = rideExtraTimeFare,
                nightShiftCharge = nightShiftCharge,
                fareParametersDetails
              }
      Nothing -> return Nothing

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
        BeamFP.rideExtraTimeFare = rideExtraTimeFare,
        BeamFP.nightShiftCharge = nightShiftCharge,
        BeamFP.fareParametersType = getFareParametersType $ FareParameters {..}
      }

findAllLateNightRides :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => [Id FareParameters] -> m Int
findAllLateNightRides fareParametersIds = findAllWithKV [Se.Is BeamFP.id $ Se.In $ getId <$> fareParametersIds, Se.Is BeamFP.nightShiftCharge $ Se.Not $ Se.Eq Nothing] <&> length

findDriverSelectedFareEarnings :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => [Id FareParameters] -> m Int
findDriverSelectedFareEarnings fareParamIds = do
  dsEarnings <- findAllWithKV [Se.Is BeamFP.id $ Se.In $ getId <$> fareParamIds] <&> (driverSelectedFare <$>)
  pure $ sum (getMoney <$> catMaybes dsEarnings)

findCustomerExtraFees :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => [Id FareParameters] -> m Int
findCustomerExtraFees fareParamIds = do
  csFees <- findAllWithKV [Se.Is BeamFP.id $ Se.In $ getId <$> fareParamIds] <&> (customerExtraFee <$>)
  pure $ sum (getMoney <$> catMaybes csFees)
