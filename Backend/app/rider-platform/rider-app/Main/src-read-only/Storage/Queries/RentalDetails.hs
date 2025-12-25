{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.RentalDetails where

import qualified Domain.Types.RentalDetails
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.RentalDetails as Beam
import qualified Storage.Queries.Transformers.RentalDetails

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.RentalDetails.RentalDetails -> m ())
create = createWithKV

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.RentalDetails.RentalDetails -> m (Maybe Domain.Types.RentalDetails.RentalDetails))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

instance FromTType' Beam.RentalDetails Domain.Types.RentalDetails.RentalDetails where
  fromTType' (Beam.RentalDetailsT {..}) = do
    pure $
      Just
        Domain.Types.RentalDetails.RentalDetails
          { baseFare = Kernel.Utils.Common.mkPriceWithDefault baseFareAmount currency baseFare,
            deadKmFare = Kernel.Utils.Common.mkPrice currency deadKmFare,
            id = Kernel.Types.Id.Id id,
            includedDistancePerHr = Kernel.Utils.Common.mkDistanceWithDefaultMeters distanceUnit includedDistancePerHrValue $ Kernel.Utils.Common.kilometersToMeters includedKmPerHr,
            nightShiftInfo = Storage.Queries.Transformers.RentalDetails.mkNightShiftInfo nightShiftCharge nightShiftChargeAmount nightShiftEnd nightShiftStart currency,
            perExtraKmRate = Kernel.Utils.Common.mkPriceWithDefault perExtraKmRateAmount currency perExtraKmRate,
            perExtraMinRate = Kernel.Utils.Common.mkPriceWithDefault perExtraMinRateAmount currency perExtraMinRate,
            perHourCharge = Kernel.Utils.Common.mkPriceWithDefault perHourChargeAmount currency perHourCharge,
            plannedPerKmRate = Kernel.Utils.Common.mkPriceWithDefault plannedPerKmRateAmount currency plannedPerKmRate
          }

instance ToTType' Beam.RentalDetails Domain.Types.RentalDetails.RentalDetails where
  toTType' (Domain.Types.RentalDetails.RentalDetails {..}) = do
    Beam.RentalDetailsT
      { Beam.baseFare = (.amountInt) baseFare,
        Beam.baseFareAmount = Just $ (.amount) baseFare,
        Beam.currency = Just $ (.currency) baseFare,
        Beam.deadKmFare = (.amount) deadKmFare,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.distanceUnit = Just $ (.unit) includedDistancePerHr,
        Beam.includedDistancePerHrValue = Just $ Kernel.Utils.Common.distanceToHighPrecDistance ((.unit) includedDistancePerHr) includedDistancePerHr,
        Beam.includedKmPerHr = Kernel.Utils.Common.metersToKilometers $ Kernel.Utils.Common.distanceToMeters includedDistancePerHr,
        Beam.nightShiftCharge = (.amountInt) . (.nightShiftCharge) <$> nightShiftInfo,
        Beam.nightShiftChargeAmount = (.amount) . (.nightShiftCharge) <$> nightShiftInfo,
        Beam.nightShiftEnd = (.nightShiftEnd) <$> nightShiftInfo,
        Beam.nightShiftStart = (.nightShiftStart) <$> nightShiftInfo,
        Beam.perExtraKmRate = (.amountInt) perExtraKmRate,
        Beam.perExtraKmRateAmount = Just $ (.amount) perExtraKmRate,
        Beam.perExtraMinRate = (.amountInt) perExtraMinRate,
        Beam.perExtraMinRateAmount = Just $ (.amount) perExtraMinRate,
        Beam.perHourCharge = (.amountInt) perHourCharge,
        Beam.perHourChargeAmount = Just $ (.amount) perHourCharge,
        Beam.plannedPerKmRate = (.amountInt) plannedPerKmRate,
        Beam.plannedPerKmRateAmount = Just $ (.amount) plannedPerKmRate
      }
