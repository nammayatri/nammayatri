{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.RentalDetails where

import qualified Domain.Types.Extra.RentalDetails
import qualified Domain.Types.RentalDetails
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Types.Distance
import Kernel.Types.Error
import qualified Kernel.Types.Id
import qualified Kernel.Types.Price
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.RentalDetails as Beam
import qualified Storage.Queries.Transformers.RentalDetails

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Domain.Types.RentalDetails.RentalDetails -> m ()
create = createWithKV

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Kernel.Types.Id.Id Domain.Types.RentalDetails.RentalDetails -> m (Maybe Domain.Types.RentalDetails.RentalDetails)
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
      { Beam.baseFare = Kernel.Types.Price.getPriceAmountInt baseFare,
        Beam.baseFareAmount = Just $ Kernel.Types.Price.getPriceAmount baseFare,
        Beam.currency = Just $ Kernel.Types.Price.getPriceCurrency baseFare,
        Beam.deadKmFare = Kernel.Types.Price.getPriceAmount deadKmFare,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.distanceUnit = Just $ Kernel.Types.Distance.unit includedDistancePerHr,
        Beam.includedDistancePerHrValue = Just $ Kernel.Utils.Common.distanceToHighPrecDistance (Kernel.Types.Distance.unit includedDistancePerHr) includedDistancePerHr,
        Beam.includedKmPerHr = Kernel.Utils.Common.metersToKilometers $ Kernel.Utils.Common.distanceToMeters includedDistancePerHr,
        Beam.nightShiftCharge = nightShiftInfo <&> (Kernel.Types.Price.getPriceAmountInt . Domain.Types.Extra.RentalDetails.getNightShiftInfoCharge),
        Beam.nightShiftChargeAmount = nightShiftInfo <&> (Kernel.Types.Price.getPriceAmount . Domain.Types.Extra.RentalDetails.getNightShiftInfoCharge),
        Beam.nightShiftEnd = nightShiftInfo <&> Domain.Types.Extra.RentalDetails.getNightShiftInfoEnd,
        Beam.nightShiftStart = nightShiftInfo <&> Domain.Types.Extra.RentalDetails.getNightShiftInfoStart,
        Beam.perExtraKmRate = Kernel.Types.Price.getPriceAmountInt perExtraKmRate,
        Beam.perExtraKmRateAmount = Just $ Kernel.Types.Price.getPriceAmount perExtraKmRate,
        Beam.perExtraMinRate = Kernel.Types.Price.getPriceAmountInt perExtraMinRate,
        Beam.perExtraMinRateAmount = Just $ Kernel.Types.Price.getPriceAmount perExtraMinRate,
        Beam.perHourCharge = Kernel.Types.Price.getPriceAmountInt perHourCharge,
        Beam.perHourChargeAmount = Just $ Kernel.Types.Price.getPriceAmount perHourCharge,
        Beam.plannedPerKmRate = Kernel.Types.Price.getPriceAmountInt plannedPerKmRate,
        Beam.plannedPerKmRateAmount = Just $ Kernel.Types.Price.getPriceAmount plannedPerKmRate
      }
