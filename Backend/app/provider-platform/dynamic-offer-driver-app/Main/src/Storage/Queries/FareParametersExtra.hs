{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FareParametersExtra where

import Domain.Types.FareParameters
import qualified Domain.Types.FareParametersAmbulanceDetails as DSLFPAD
import qualified Domain.Types.FareParametersInterCityDetails as DSLFPICD
import qualified Domain.Types.FareParametersProgressiveDetails as DSLFPPD
import qualified Domain.Types.FareParametersRentalDetails as DSLFPRD
import qualified Domain.Types.FareParametersSlabDetails as DSLFPSD
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FareParameters as BeamFP
import qualified Storage.Queries.FareParametersAmbulanceDetails as BeamFPAD
import qualified Storage.Queries.FareParametersInterCityDetails as QFPICD
import qualified Storage.Queries.FareParametersProgressiveDetails as QFPPD
import qualified Storage.Queries.FareParametersRentalDetails as QFPRD
import qualified Storage.Queries.FareParametersSlabDetails as QFPSD
import Storage.Queries.OrphanInstances.FareParameters

create :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => FareParameters -> m ()
create fareParameters = do
  createWithKV fareParameters
  let fpId = getId fareParameters.id
  case fareParameters.fareParametersDetails of
    ProgressiveDetails FParamsProgressiveDetails {..} -> QFPPD.create DSLFPPD.FareParametersProgressiveDetails {fareParametersId = fpId, ..}
    SlabDetails FParamsSlabDetails {..} -> QFPSD.create DSLFPSD.FareParametersSlabDetails {fareParametersId = fpId, ..}
    RentalDetails FParamsRentalDetails {..} -> QFPRD.create DSLFPRD.FareParametersRentalDetails {fareParametersId = fpId, ..}
    InterCityDetails FParamsInterCityDetails {..} -> QFPICD.create DSLFPICD.FareParametersInterCityDetails {fareParametersId = fpId, ..}
    AmbulanceDetails FParamsAmbulanceDetails {..} -> BeamFPAD.create DSLFPAD.FareParametersAmbulanceDetails {fareParametersId = fpId, ..}

updateCancellationCharges :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Maybe HighPrecMoney -> Maybe HighPrecMoney -> Id FareParameters -> m ()
updateCancellationCharges cancellationFeeTaxExclusive cancellationTax (Id fareParametersId) =
  updateOneWithKV
    [ Se.Set BeamFP.cancellationFeeTaxExclusive cancellationFeeTaxExclusive,
      Se.Set BeamFP.cancellationTax cancellationTax
    ]
    [Se.Is BeamFP.id $ Se.Eq fareParametersId]

updateTdsDeduction ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Maybe HighPrecMoney ->
  Maybe Double ->
  UTCTime ->
  Id FareParameters ->
  m ()
updateTdsDeduction tdsAmount tdsRate processedAt fareParametersId =
  updateOneWithKV
    [ Se.Set BeamFP.tdsAmount tdsAmount,
      Se.Set BeamFP.tdsRate tdsRate,
      Se.Set BeamFP.tdsProcessedAt (Just processedAt)
    ]
    [Se.Is BeamFP.id $ Se.Eq (getId fareParametersId)]

updateFareParameters :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => FareParameters -> Id FareParameters -> m ()
updateFareParameters FareParameters {..} id_ = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamFP.driverSelectedFare $ roundToIntegral <$> driverSelectedFare,
      Se.Set BeamFP.driverSelectedFareAmount driverSelectedFare,
      Se.Set BeamFP.customerExtraFee $ roundToIntegral <$> customerExtraFee,
      Se.Set BeamFP.customerExtraFeeAmount customerExtraFee,
      Se.Set BeamFP.negativeFareAdjustment $ roundToIntegral <$> negativeFareAdjustment,
      Se.Set BeamFP.negativeFareAdjustmentAmount negativeFareAdjustment,
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
      Se.Set BeamFP.negotiatedFareDelta negotiatedFareDelta,
      Se.Set BeamFP.updatedAt (Just now)
    ]
    [Se.Is BeamFP.id (Se.Eq id_.getId)]
  case fareParametersDetails of
    AmbulanceDetails fpadt -> void $ BeamFPAD.update id_ fpadt
    _ -> pure ()

findAllLateNightRides :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => [Id FareParameters] -> m Int
findAllLateNightRides fareParametersIds = findAllWithKV [Se.Is BeamFP.id $ Se.In $ getId <$> fareParametersIds, Se.Is BeamFP.nightShiftCharge $ Se.Not $ Se.Eq Nothing] <&> length

findDriverSelectedFareEarnings :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => [Id FareParameters] -> m HighPrecMoney
findDriverSelectedFareEarnings fareParamIds = do
  dsEarnings <- findAllWithKV [Se.Is BeamFP.id $ Se.In $ getId <$> fareParamIds] <&> (driverSelectedFare <$>)
  pure $ sum (catMaybes dsEarnings)

findCustomerExtraFees :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => [Id FareParameters] -> m HighPrecMoney
findCustomerExtraFees fareParamIds = do
  csFees <- findAllWithKV [Se.Is BeamFP.id $ Se.In $ getId <$> fareParamIds] <&> (customerExtraFee <$>)
  pure $ sum (catMaybes csFees)
