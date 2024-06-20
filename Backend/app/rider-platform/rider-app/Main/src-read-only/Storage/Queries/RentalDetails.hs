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
          { id = Kernel.Types.Id.Id id,
            baseFare = Kernel.Utils.Common.mkPriceWithDefault baseFareAmount currency baseFare,
            perHourCharge = Kernel.Utils.Common.mkPriceWithDefault perHourChargeAmount currency perHourCharge,
            perExtraMinRate = Kernel.Utils.Common.mkPriceWithDefault perExtraMinRateAmount currency perExtraMinRate,
            perExtraKmRate = Kernel.Utils.Common.mkPriceWithDefault perExtraKmRateAmount currency perExtraKmRate,
            includedDistancePerHr = Kernel.Utils.Common.mkDistanceWithDefaultMeters distanceUnit includedDistancePerHrValue $ Kernel.Utils.Common.kilometersToMeters includedKmPerHr,
            plannedPerKmRate = Kernel.Utils.Common.mkPriceWithDefault plannedPerKmRateAmount currency plannedPerKmRate,
            deadKmFare = Kernel.Utils.Common.mkPrice currency deadKmFare,
            nightShiftInfo = Storage.Queries.Transformers.RentalDetails.mkNightShiftInfo nightShiftCharge nightShiftChargeAmount nightShiftEnd nightShiftStart currency
          }

instance ToTType' Beam.RentalDetails Domain.Types.RentalDetails.RentalDetails where
  toTType' (Domain.Types.RentalDetails.RentalDetails {..}) = do
    Beam.RentalDetailsT
      { Beam.id = Kernel.Types.Id.getId id,
        Beam.baseFare = (.amountInt) baseFare,
        Beam.baseFareAmount = Just $ (.amount) baseFare,
        Beam.currency = Just $ (.currency) baseFare,
        Beam.perHourCharge = (.amountInt) perHourCharge,
        Beam.perHourChargeAmount = Just $ (.amount) perHourCharge,
        Beam.perExtraMinRate = (.amountInt) perExtraMinRate,
        Beam.perExtraMinRateAmount = Just $ (.amount) perExtraMinRate,
        Beam.perExtraKmRate = (.amountInt) perExtraKmRate,
        Beam.perExtraKmRateAmount = Just $ (.amount) perExtraKmRate,
        Beam.distanceUnit = Just $ (.unit) includedDistancePerHr,
        Beam.includedDistancePerHrValue = Just $ Kernel.Utils.Common.distanceToHighPrecDistance ((.unit) includedDistancePerHr) includedDistancePerHr,
        Beam.includedKmPerHr = Kernel.Utils.Common.metersToKilometers $ Kernel.Utils.Common.distanceToMeters includedDistancePerHr,
        Beam.plannedPerKmRate = (.amountInt) plannedPerKmRate,
        Beam.plannedPerKmRateAmount = Just $ (.amount) plannedPerKmRate,
        Beam.deadKmFare = (.amount) deadKmFare,
        Beam.nightShiftCharge = (.amountInt) . (.nightShiftCharge) <$> nightShiftInfo,
        Beam.nightShiftChargeAmount = (.amount) . (.nightShiftCharge) <$> nightShiftInfo,
        Beam.nightShiftEnd = (.nightShiftEnd) <$> nightShiftInfo,
        Beam.nightShiftStart = (.nightShiftStart) <$> nightShiftInfo
      }
