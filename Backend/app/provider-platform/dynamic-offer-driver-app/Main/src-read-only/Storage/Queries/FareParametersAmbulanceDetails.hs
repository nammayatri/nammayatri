{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FareParametersAmbulanceDetails (module Storage.Queries.FareParametersAmbulanceDetails, module ReExport) where

import qualified Domain.Types.FareParametersAmbulanceDetails
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FareParametersAmbulanceDetails as Beam
import Storage.Queries.FareParametersAmbulanceDetailsExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FareParametersAmbulanceDetails.FareParametersAmbulanceDetails -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.FareParametersAmbulanceDetails.FareParametersAmbulanceDetails] -> m ())
createMany = traverse_ create

findByFareParametersId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m (Maybe Domain.Types.FareParametersAmbulanceDetails.FareParametersAmbulanceDetails))
findByFareParametersId fareParametersId = do findOneWithKV [Se.Is Beam.fareParametersId $ Se.Eq fareParametersId]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m (Maybe Domain.Types.FareParametersAmbulanceDetails.FareParametersAmbulanceDetails))
findByPrimaryKey fareParametersId = do findOneWithKV [Se.And [Se.Is Beam.fareParametersId $ Se.Eq fareParametersId]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FareParametersAmbulanceDetails.FareParametersAmbulanceDetails -> m ())
updateByPrimaryKey (Domain.Types.FareParametersAmbulanceDetails.FareParametersAmbulanceDetails {..}) = do
  updateWithKV
    [ Se.Set Beam.cgst cgst,
      Se.Set Beam.currency currency,
      Se.Set Beam.distBasedFare distBasedFare,
      Se.Set Beam.platformFee platformFee,
      Se.Set Beam.sgst sgst
    ]
    [Se.And [Se.Is Beam.fareParametersId $ Se.Eq fareParametersId]]
