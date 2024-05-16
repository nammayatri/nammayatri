{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.VehicleDetails (module Storage.Queries.VehicleDetails, module ReExport) where

import qualified Data.Text
import qualified Domain.Types.VehicleDetails
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (KvDbFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.VehicleDetails as Beam
import Storage.Queries.VehicleDetailsExtra as ReExport

create :: KvDbFlow m r => (Domain.Types.VehicleDetails.VehicleDetails -> m ())
create = createWithKV

createMany :: KvDbFlow m r => ([Domain.Types.VehicleDetails.VehicleDetails] -> m ())
createMany = traverse_ create

findById :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.VehicleDetails.VehicleDetails -> m (Maybe Domain.Types.VehicleDetails.VehicleDetails))
findById (Kernel.Types.Id.Id id) = do findOneWithKV [Se.Is Beam.id $ Se.Eq id]

findByMake :: KvDbFlow m r => (Data.Text.Text -> m [Domain.Types.VehicleDetails.VehicleDetails])
findByMake make = do findAllWithKV [Se.Is Beam.make $ Se.Eq make]

findByMakeAndModel :: KvDbFlow m r => (Data.Text.Text -> Data.Text.Text -> m (Maybe Domain.Types.VehicleDetails.VehicleDetails))
findByMakeAndModel make model = do findOneWithKV [Se.And [Se.Is Beam.make $ Se.Eq make, Se.Is Beam.model $ Se.Eq model]]

findByPrimaryKey :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.VehicleDetails.VehicleDetails -> m (Maybe Domain.Types.VehicleDetails.VehicleDetails))
findByPrimaryKey (Kernel.Types.Id.Id id) = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq id]]

updateByPrimaryKey :: KvDbFlow m r => (Domain.Types.VehicleDetails.VehicleDetails -> m ())
updateByPrimaryKey (Domain.Types.VehicleDetails.VehicleDetails {..}) = do
  updateWithKV
    [ Se.Set Beam.acAvailable acAvailable,
      Se.Set Beam.capacity capacity,
      Se.Set Beam.make make,
      Se.Set Beam.model model,
      Se.Set Beam.vehicleVariant vehicleVariant
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
