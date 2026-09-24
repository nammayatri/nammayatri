{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FareParametersRentalDetails (module Storage.Queries.FareParametersRentalDetails, module ReExport) where

import qualified Domain.Types.FareParametersRentalDetails
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FareParametersRentalDetails as Beam
import Storage.Queries.FareParametersRentalDetailsExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FareParametersRentalDetails.FareParametersRentalDetails -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.FareParametersRentalDetails.FareParametersRentalDetails] -> m ())
createMany = traverse_ create

findByFareParametersId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m (Maybe Domain.Types.FareParametersRentalDetails.FareParametersRentalDetails))
findByFareParametersId fareParametersId = do findOneWithKV [Se.Is Beam.fareParametersId $ Se.Eq fareParametersId]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m (Maybe Domain.Types.FareParametersRentalDetails.FareParametersRentalDetails))
findByPrimaryKey fareParametersId = do findOneWithKV [Se.And [Se.Is Beam.fareParametersId $ Se.Eq fareParametersId]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FareParametersRentalDetails.FareParametersRentalDetails -> m ())
updateByPrimaryKey (Domain.Types.FareParametersRentalDetails.FareParametersRentalDetails {..}) = do
  updateWithKV
    [ Se.Set Beam.currency (Kernel.Prelude.Just currency),
      Se.Set Beam.deadKmFare (Kernel.Prelude.Just deadKmFare),
      Se.Set Beam.distBasedFare (Kernel.Prelude.roundToIntegral distBasedFare),
      Se.Set Beam.distBasedFareAmount (Kernel.Prelude.Just distBasedFare),
      Se.Set Beam.distanceUnit (Kernel.Prelude.Just distanceUnit),
      Se.Set Beam.extraDistance (Kernel.Prelude.Just extraDistance),
      Se.Set Beam.extraDuration (Kernel.Prelude.Just extraDuration),
      Se.Set Beam.timeBasedFare (Kernel.Prelude.roundToIntegral timeBasedFare),
      Se.Set Beam.timeBasedFareAmount (Kernel.Prelude.Just timeBasedFare)
    ]
    [Se.And [Se.Is Beam.fareParametersId $ Se.Eq fareParametersId]]
