{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FareParametersInterCityDetails (module Storage.Queries.FareParametersInterCityDetails, module ReExport) where

import qualified Domain.Types.FareParametersInterCityDetails
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FareParametersInterCityDetails as Beam
import Storage.Queries.FareParametersInterCityDetailsExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FareParametersInterCityDetails.FareParametersInterCityDetails -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.FareParametersInterCityDetails.FareParametersInterCityDetails] -> m ())
createMany = traverse_ create

findByFareParametersId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m (Maybe Domain.Types.FareParametersInterCityDetails.FareParametersInterCityDetails))
findByFareParametersId fareParametersId = do findOneWithKV [Se.Is Beam.fareParametersId $ Se.Eq fareParametersId]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m (Maybe Domain.Types.FareParametersInterCityDetails.FareParametersInterCityDetails))
findByPrimaryKey fareParametersId = do findOneWithKV [Se.And [Se.Is Beam.fareParametersId $ Se.Eq fareParametersId]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FareParametersInterCityDetails.FareParametersInterCityDetails -> m ())
updateByPrimaryKey (Domain.Types.FareParametersInterCityDetails.FareParametersInterCityDetails {..}) = do
  updateWithKV
    [ Se.Set Beam.currency currency,
      Se.Set Beam.distanceFare distanceFare,
      Se.Set Beam.extraDistanceFare extraDistanceFare,
      Se.Set Beam.extraTimeFare extraTimeFare,
      Se.Set Beam.pickupCharge pickupCharge,
      Se.Set Beam.stateEntryPermitCharges stateEntryPermitCharges,
      Se.Set Beam.timeFare timeFare
    ]
    [Se.And [Se.Is Beam.fareParametersId $ Se.Eq fareParametersId]]
