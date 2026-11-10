{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.InterCityDetails where

import qualified Domain.Types.Extra.RentalDetails
import qualified Domain.Types.InterCityDetails
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Distance
import Kernel.Types.Error
import qualified Kernel.Types.Id
import qualified Kernel.Types.Price
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.InterCityDetails as Beam
import qualified Storage.Queries.Transformers.RentalDetails

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Domain.Types.InterCityDetails.InterCityDetails -> m ()
create = createWithKV

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Kernel.Types.Id.Id Domain.Types.InterCityDetails.InterCityDetails -> m (Maybe Domain.Types.InterCityDetails.InterCityDetails)
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

instance FromTType' Beam.InterCityDetails Domain.Types.InterCityDetails.InterCityDetails where
  fromTType' (Beam.InterCityDetailsT {..}) = do
    pure $
      Just
        Domain.Types.InterCityDetails.InterCityDetails
          { baseFare = Kernel.Utils.Common.mkPrice (Just currency) baseFare,
            deadKmFare = Kernel.Utils.Common.mkPrice (Just currency) deadKmFare,
            id = Kernel.Types.Id.Id id,
            kmPerPlannedExtraHour = Kernel.Utils.Common.Distance kmPerPlannedExtraHour distanceUnit,
            nightShiftInfo = Storage.Queries.Transformers.RentalDetails.mkNightShiftInfo (Kernel.Prelude.roundToIntegral <$> nightShiftCharge) nightShiftCharge nightShiftEnd nightShiftStart (Just currency),
            perDayMaxAllowanceInMins = perDayMaxAllowanceInMins,
            perDayMaxHourAllowance = perDayMaxHourAllowance,
            perExtraKmRate = Kernel.Utils.Common.mkPrice (Just currency) perExtraKmRate,
            perExtraMinRate = Kernel.Utils.Common.mkPrice (Just currency) perExtraMinRate,
            perHourCharge = Kernel.Utils.Common.mkPrice (Just currency) perHourCharge,
            plannedPerKmRateOneWay = Kernel.Utils.Common.mkPrice (Just currency) plannedPerKmRateOneWay,
            plannedPerKmRateRoundTrip = Kernel.Utils.Common.mkPrice (Just currency) plannedPerKmRateRoundTrip,
            roundTrip = roundTrip,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.InterCityDetails Domain.Types.InterCityDetails.InterCityDetails where
  toTType' (Domain.Types.InterCityDetails.InterCityDetails {..}) = do
    Beam.InterCityDetailsT
      { Beam.baseFare = Kernel.Types.Price.getPriceAmount baseFare,
        Beam.currency = Kernel.Types.Price.getPriceCurrency baseFare,
        Beam.deadKmFare = Kernel.Types.Price.getPriceAmount deadKmFare,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.distanceUnit = Kernel.Types.Distance.unit kmPerPlannedExtraHour,
        Beam.kmPerPlannedExtraHour = Kernel.Utils.Common.distanceToHighPrecDistance (Kernel.Types.Distance.unit kmPerPlannedExtraHour) kmPerPlannedExtraHour,
        Beam.nightShiftCharge = nightShiftInfo <&> (Kernel.Types.Price.getPriceAmount . Domain.Types.Extra.RentalDetails.getNightShiftInfoCharge),
        Beam.nightShiftEnd = nightShiftInfo <&> Domain.Types.Extra.RentalDetails.getNightShiftInfoEnd,
        Beam.nightShiftStart = nightShiftInfo <&> Domain.Types.Extra.RentalDetails.getNightShiftInfoStart,
        Beam.perDayMaxAllowanceInMins = perDayMaxAllowanceInMins,
        Beam.perDayMaxHourAllowance = perDayMaxHourAllowance,
        Beam.perExtraKmRate = Kernel.Types.Price.getPriceAmount perExtraKmRate,
        Beam.perExtraMinRate = Kernel.Types.Price.getPriceAmount perExtraMinRate,
        Beam.perHourCharge = Kernel.Types.Price.getPriceAmount perHourCharge,
        Beam.plannedPerKmRateOneWay = Kernel.Types.Price.getPriceAmount plannedPerKmRateOneWay,
        Beam.plannedPerKmRateRoundTrip = Kernel.Types.Price.getPriceAmount plannedPerKmRateRoundTrip,
        Beam.roundTrip = roundTrip,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
