{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.VendorSplitDetailsExtra where

import Domain.Types.MerchantOperatingCity
import Domain.Types.VehicleCategory
import Domain.Types.VendorSplitDetails
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import Lib.Types.SpecialLocation
import qualified Sequelize as Se
import qualified Storage.Beam.VendorSplitDetails as Beam
import Storage.Queries.OrphanInstances.VendorSplitDetails

-- Extra code goes here --

findAllByAreaCityAndCategory ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Area ->
  Id MerchantOperatingCity ->
  VehicleCategory ->
  m [VendorSplitDetails]
findAllByAreaCityAndCategory area merchantOperatingCityId vehicleCategory =
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.area $ Se.Eq area,
          Se.Is Beam.merchantOperatingCityId $ Se.Eq (getId merchantOperatingCityId),
          Se.Is Beam.vehicleCategory $ Se.Eq vehicleCategory
        ]
    ]
