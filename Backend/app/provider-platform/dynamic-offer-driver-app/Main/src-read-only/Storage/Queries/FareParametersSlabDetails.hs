{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FareParametersSlabDetails (module Storage.Queries.FareParametersSlabDetails, module ReExport) where

import qualified Domain.Types.FareParametersSlabDetails
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FareParametersSlabDetails as Beam
import Storage.Queries.FareParametersSlabDetailsExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FareParametersSlabDetails.FareParametersSlabDetails -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.FareParametersSlabDetails.FareParametersSlabDetails] -> m ())
createMany = traverse_ create

findByFareParametersId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m (Maybe Domain.Types.FareParametersSlabDetails.FareParametersSlabDetails))
findByFareParametersId fareParametersId = do findOneWithKV [Se.Is Beam.fareParametersId $ Se.Eq fareParametersId]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m (Maybe Domain.Types.FareParametersSlabDetails.FareParametersSlabDetails))
findByPrimaryKey fareParametersId = do findOneWithKV [Se.And [Se.Is Beam.fareParametersId $ Se.Eq fareParametersId]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FareParametersSlabDetails.FareParametersSlabDetails -> m ())
updateByPrimaryKey (Domain.Types.FareParametersSlabDetails.FareParametersSlabDetails {..}) = do
  updateWithKV
    [ Se.Set Beam.cgst cgst,
      Se.Set Beam.currency (Kernel.Prelude.Just currency),
      Se.Set Beam.platformFee platformFee,
      Se.Set Beam.sgst sgst
    ]
    [Se.And [Se.Is Beam.fareParametersId $ Se.Eq fareParametersId]]
