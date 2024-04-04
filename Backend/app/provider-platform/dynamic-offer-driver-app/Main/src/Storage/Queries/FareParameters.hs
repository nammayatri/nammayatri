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
import Storage.Queries.FareParameters.FareParametersRentalDetails as QFPRD
import qualified Storage.Queries.FareParameters.FareParametersRentalDetails as BeamFPRD
import Storage.Queries.FareParameters.FareParametersSlabDetails as QFPSD
import qualified Storage.Queries.FareParameters.FareParametersSlabDetails as BeamFPSD

create :: KvDbFlow m r => DFP.FareParameters -> m ()
create fareParameters = do
  createWithKV fareParameters
  case fareParameters.fareParametersDetails of
    ProgressiveDetails fppdt -> QFPPD.create (fareParameters.id, fppdt)
    SlabDetails fpsdt -> QFPSD.create (fareParameters.id, fpsdt)
    RentalDetails fprdt -> QFPRD.create (fareParameters.id, fprdt)

findById :: KvDbFlow m r => Id FareParameters -> m (Maybe FareParameters)
findById (Id fareParametersId) = findOneWithKV [Se.Is BeamFP.id $ Se.Eq fareParametersId]

findAllIn :: KvDbFlow m r => [Id FareParameters] -> m [FareParameters]
findAllIn fareParametersIds = findAllWithKV [Se.Is BeamFP.id $ Se.In $ getId <$> fareParametersIds]

updateFareParameters :: KvDbFlow m r => FareParameters -> m ()
updateFareParameters FareParameters {..} = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamFP.driverSelectedFare $ roundToIntegral <$> driverSelectedFare,
      Se.Set BeamFP.driverSelectedFareAmount driverSelectedFare,
      Se.Set BeamFP.customerExtraFee $ roundToIntegral <$> customerExtraFee,
      Se.Set BeamFP.customerExtraFeeAmount customerExtraFee,
      Se.Set BeamFP.serviceCharge $ roundToIntegral <$> serviceCharge,
      Se.Set BeamFP.serviceChargeAmount serviceCharge,
      Se.Set BeamFP.govtCharges $ roundToIntegral <$> govtCharges,
      Se.Set BeamFP.govtChargesAmount govtCharges,
      Se.Set BeamFP.nightShiftRateIfApplies nightShiftRateIfApplies,
      Se.Set BeamFP.baseFare $ roundToIntegral baseFare,
      Se.Set BeamFP.baseFareAmount $ Just baseFare,
      Se.Set BeamFP.waitingCharge $ roundToIntegral <$> waitingCharge,
      Se.Set BeamFP.waitingChargeAmount waitingCharge,
      Se.Set BeamFP.rideExtraTimeFare $ roundToIntegral <$> rideExtraTimeFare,
      Se.Set BeamFP.rideExtraTimeFareAmount rideExtraTimeFare,
      Se.Set BeamFP.nightShiftCharge $ roundToIntegral <$> nightShiftCharge,
      Se.Set BeamFP.nightShiftChargeAmount nightShiftCharge,
      Se.Set BeamFP.currency $ Just currency,
      Se.Set BeamFP.updatedAt (Just now)
    ]
    [Se.Is BeamFP.id (Se.Eq id.getId)]

findAllLateNightRides :: KvDbFlow m r => [Id FareParameters] -> m Int
findAllLateNightRides fareParametersIds = findAllWithKV [Se.Is BeamFP.id $ Se.In $ getId <$> fareParametersIds, Se.Is BeamFP.nightShiftCharge $ Se.Not $ Se.Eq Nothing] <&> length

findDriverSelectedFareEarnings :: KvDbFlow m r => [Id FareParameters] -> m HighPrecMoney
findDriverSelectedFareEarnings fareParamIds = do
  dsEarnings <- findAllWithKV [Se.Is BeamFP.id $ Se.In $ getId <$> fareParamIds] <&> (driverSelectedFare <$>)
  pure $ sum (catMaybes dsEarnings)

findCustomerExtraFees :: KvDbFlow m r => [Id FareParameters] -> m HighPrecMoney
findCustomerExtraFees fareParamIds = do
  csFees <- findAllWithKV [Se.Is BeamFP.id $ Se.In $ getId <$> fareParamIds] <&> (customerExtraFee <$>)
  pure $ sum (catMaybes csFees)

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
        Rental -> do
          mFullFPRD <- BeamFPRD.findById' (Id id)
          case mFullFPRD of
            Just (_, fPRD) -> return (Just $ RentalDetails fPRD)
            Nothing -> return Nothing
    now <- getCurrentTime
    case mFareParametersDetails of
      Just fareParametersDetails -> do
        return $
          Just
            FareParameters
              { id = Id id,
                driverSelectedFare = mkAmountWithDefault driverSelectedFareAmount <$> driverSelectedFare,
                customerExtraFee = mkAmountWithDefault customerExtraFeeAmount <$> customerExtraFee,
                serviceCharge = mkAmountWithDefault serviceChargeAmount <$> serviceCharge,
                parkingCharge = parkingCharge,
                nightShiftRateIfApplies = nightShiftRateIfApplies,
                govtCharges = mkAmountWithDefault govtChargesAmount <$> govtCharges,
                baseFare = mkAmountWithDefault baseFareAmount baseFare,
                waitingCharge = mkAmountWithDefault waitingChargeAmount <$> waitingCharge,
                rideExtraTimeFare = mkAmountWithDefault rideExtraTimeFareAmount <$> rideExtraTimeFare,
                nightShiftCharge = mkAmountWithDefault nightShiftChargeAmount <$> nightShiftCharge,
                currency = fromMaybe INR currency,
                fareParametersDetails,
                customerCancellationDues = customerCancellationDues,
                congestionCharge = mkAmountWithDefault congestionChargeAmount <$> congestionCharge,
                tollCharges = tollCharges,
                updatedAt = fromMaybe now updatedAt
              }
      Nothing -> return Nothing

instance ToTType' BeamFP.FareParameters FareParameters where
  toTType' FareParameters {..} = do
    BeamFP.FareParametersT
      { BeamFP.id = getId id,
        BeamFP.driverSelectedFare = roundToIntegral <$> driverSelectedFare,
        BeamFP.customerExtraFee = roundToIntegral <$> customerExtraFee,
        BeamFP.serviceCharge = roundToIntegral <$> serviceCharge,
        BeamFP.parkingCharge = parkingCharge,
        BeamFP.govtCharges = roundToIntegral <$> govtCharges,
        BeamFP.driverSelectedFareAmount = driverSelectedFare,
        BeamFP.customerExtraFeeAmount = customerExtraFee,
        BeamFP.serviceChargeAmount = serviceCharge,
        BeamFP.govtChargesAmount = govtCharges,
        BeamFP.nightShiftRateIfApplies = nightShiftRateIfApplies,
        BeamFP.baseFare = roundToIntegral baseFare,
        BeamFP.waitingCharge = roundToIntegral <$> waitingCharge,
        BeamFP.rideExtraTimeFare = roundToIntegral <$> rideExtraTimeFare,
        BeamFP.nightShiftCharge = roundToIntegral <$> nightShiftCharge,
        BeamFP.baseFareAmount = Just baseFare,
        BeamFP.waitingChargeAmount = waitingCharge,
        BeamFP.rideExtraTimeFareAmount = rideExtraTimeFare,
        BeamFP.nightShiftChargeAmount = nightShiftCharge,
        BeamFP.fareParametersType = getFareParametersType $ FareParameters {..},
        BeamFP.customerCancellationDues = customerCancellationDues,
        BeamFP.tollCharges = tollCharges,
        BeamFP.congestionCharge = roundToIntegral <$> congestionCharge,
        BeamFP.congestionChargeAmount = congestionCharge,
        BeamFP.currency = Just currency,
        BeamFP.updatedAt = Just updatedAt
      }
