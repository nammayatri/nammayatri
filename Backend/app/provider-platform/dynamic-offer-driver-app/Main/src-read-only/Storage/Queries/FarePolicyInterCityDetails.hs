{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FarePolicyInterCityDetails (module Storage.Queries.FarePolicyInterCityDetails, module ReExport) where

import qualified Domain.Types.FarePolicyInterCityDetails
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FarePolicyInterCityDetails as Beam
import Storage.Queries.FarePolicyInterCityDetailsExtra as ReExport

deleteByFarePolicyId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m ())
deleteByFarePolicyId farePolicyId = do deleteWithKV [Se.Is Beam.farePolicyId $ Se.Eq farePolicyId]

findByFarePolicyId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m (Maybe Domain.Types.FarePolicyInterCityDetails.FarePolicyInterCityDetails))
findByFarePolicyId farePolicyId = do findOneWithKV [Se.Is Beam.farePolicyId $ Se.Eq farePolicyId]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m (Maybe Domain.Types.FarePolicyInterCityDetails.FarePolicyInterCityDetails))
findByPrimaryKey farePolicyId = do findOneWithKV [Se.And [Se.Is Beam.farePolicyId $ Se.Eq farePolicyId]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FarePolicyInterCityDetails.FarePolicyInterCityDetails -> m ())
updateByPrimaryKey (Domain.Types.FarePolicyInterCityDetails.FarePolicyInterCityDetails {..}) = do
  updateWithKV
    [ Se.Set Beam.baseFare baseFare,
      Se.Set Beam.currency currency,
      Se.Set Beam.deadKmFare deadKmFare,
      Se.Set Beam.defaultWaitTimeAtDestination defaultWaitTimeAtDestination,
      Se.Set Beam.kmPerPlannedExtraHour kmPerPlannedExtraHour,
      Se.Set Beam.nightShiftCharge nightShiftCharge,
      Se.Set Beam.perDayMaxAllowanceInMins perDayMaxAllowanceInMins,
      Se.Set Beam.perDayMaxHourAllowance perDayMaxHourAllowance,
      Se.Set Beam.perExtraKmRate perExtraKmRate,
      Se.Set Beam.perExtraMinRate perExtraMinRate,
      Se.Set Beam.perHourCharge perHourCharge,
      Se.Set Beam.perKmRateOneWay perKmRateOneWay,
      Se.Set Beam.perKmRateRoundTrip perKmRateRoundTrip,
      Se.Set Beam.stateEntryPermitCharges stateEntryPermitCharges,
      Se.Set Beam.freeWatingTime ((.freeWaitingTime) <$> waitingChargeInfo),
      Se.Set Beam.waitingCharge ((.waitingCharge) <$> waitingChargeInfo)
    ]
    [Se.And [Se.Is Beam.farePolicyId $ Se.Eq farePolicyId]]
