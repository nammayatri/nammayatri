{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.InterCityDetails where

import qualified Domain.Types.InterCityDetails
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.InterCityDetails as Beam
import qualified Storage.Queries.Transformers.RentalDetails

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.InterCityDetails.InterCityDetails -> m ())
create = createWithKV

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.InterCityDetails.InterCityDetails -> m (Maybe Domain.Types.InterCityDetails.InterCityDetails))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

instance FromTType' Beam.InterCityDetails Domain.Types.InterCityDetails.InterCityDetails where
  fromTType' (Beam.InterCityDetailsT {..}) = do
    pure $
      Just
        Domain.Types.InterCityDetails.InterCityDetails
          { id = Kernel.Types.Id.Id id,
            baseFare = Kernel.Utils.Common.mkPrice (Just currency) baseFare,
            perHourCharge = Kernel.Utils.Common.mkPrice (Just currency) perHourCharge,
            perExtraMinRate = Kernel.Utils.Common.mkPrice (Just currency) perExtraMinRate,
            perExtraKmRate = Kernel.Utils.Common.mkPrice (Just currency) perExtraKmRate,
            deadKmFare = Kernel.Utils.Common.mkPrice (Just currency) deadKmFare,
            kmPerPlannedExtraHour = Kernel.Utils.Common.Distance kmPerPlannedExtraHour distanceUnit,
            plannedPerKmRateOneWay = Kernel.Utils.Common.mkPrice (Just currency) plannedPerKmRateOneWay,
            plannedPerKmRateRoundTrip = Kernel.Utils.Common.mkPrice (Just currency) plannedPerKmRateRoundTrip,
            perDayMaxHourAllowance = perDayMaxHourAllowance,
            nightShiftInfo = Storage.Queries.Transformers.RentalDetails.mkNightShiftInfo (Kernel.Prelude.roundToIntegral <$> nightShiftCharge) nightShiftCharge nightShiftEnd nightShiftStart (Just currency),
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.InterCityDetails Domain.Types.InterCityDetails.InterCityDetails where
  toTType' (Domain.Types.InterCityDetails.InterCityDetails {..}) = do
    Beam.InterCityDetailsT
      { Beam.id = Kernel.Types.Id.getId id,
        Beam.baseFare = (.amount) baseFare,
        Beam.currency = (.currency) baseFare,
        Beam.perHourCharge = (.amount) perHourCharge,
        Beam.perExtraMinRate = (.amount) perExtraMinRate,
        Beam.perExtraKmRate = (.amount) perExtraKmRate,
        Beam.deadKmFare = (.amount) deadKmFare,
        Beam.distanceUnit = (.unit) kmPerPlannedExtraHour,
        Beam.kmPerPlannedExtraHour = Kernel.Utils.Common.distanceToHighPrecDistance ((.unit) kmPerPlannedExtraHour) kmPerPlannedExtraHour,
        Beam.plannedPerKmRateOneWay = (.amount) plannedPerKmRateOneWay,
        Beam.plannedPerKmRateRoundTrip = (.amount) plannedPerKmRateRoundTrip,
        Beam.perDayMaxHourAllowance = perDayMaxHourAllowance,
        Beam.nightShiftCharge = (.amount) . (.nightShiftCharge) <$> nightShiftInfo,
        Beam.nightShiftEnd = (.nightShiftEnd) <$> nightShiftInfo,
        Beam.nightShiftStart = (.nightShiftStart) <$> nightShiftInfo,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
