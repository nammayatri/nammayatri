{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FareParametersProgressiveDetails (module Storage.Queries.FareParametersProgressiveDetails, module ReExport) where

import qualified Domain.Types.FareParametersProgressiveDetails
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FareParametersProgressiveDetails as Beam
import Storage.Queries.FareParametersProgressiveDetailsExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FareParametersProgressiveDetails.FareParametersProgressiveDetails -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.FareParametersProgressiveDetails.FareParametersProgressiveDetails] -> m ())
createMany = traverse_ create

findByFareParametersId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m (Maybe Domain.Types.FareParametersProgressiveDetails.FareParametersProgressiveDetails))
findByFareParametersId fareParametersId = do findOneWithKV [Se.Is Beam.fareParametersId $ Se.Eq fareParametersId]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m (Maybe Domain.Types.FareParametersProgressiveDetails.FareParametersProgressiveDetails))
findByPrimaryKey fareParametersId = do findOneWithKV [Se.And [Se.Is Beam.fareParametersId $ Se.Eq fareParametersId]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FareParametersProgressiveDetails.FareParametersProgressiveDetails -> m ())
updateByPrimaryKey (Domain.Types.FareParametersProgressiveDetails.FareParametersProgressiveDetails {..}) = do
  updateWithKV
    [ Se.Set Beam.currency (Kernel.Prelude.Just currency),
      Se.Set Beam.deadKmFare (Kernel.Prelude.roundToIntegral deadKmFare),
      Se.Set Beam.deadKmFareAmount (Kernel.Prelude.Just deadKmFare),
      Se.Set Beam.extraKmFare (Kernel.Prelude.roundToIntegral <$> extraKmFare),
      Se.Set Beam.extraKmFareAmount extraKmFare,
      Se.Set Beam.rideDurationFare rideDurationFare
    ]
    [Se.And [Se.Is Beam.fareParametersId $ Se.Eq fareParametersId]]
