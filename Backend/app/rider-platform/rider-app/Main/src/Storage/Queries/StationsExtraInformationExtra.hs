{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.StationsExtraInformationExtra where

import Domain.Types.MerchantOperatingCity
import Domain.Types.StationsExtraInformation
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.StationsExtraInformation as Beam
import Storage.Queries.OrphanInstances.StationsExtraInformation

-- Extra code goes here --

getBystationIdsAndCity ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  [Kernel.Prelude.Text] ->
  Kernel.Types.Id.Id MerchantOperatingCity ->
  m [Domain.Types.StationsExtraInformation.StationsExtraInformation]
getBystationIdsAndCity stationIds city =
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq city.getId,
          Se.Is Beam.stationId $ Se.In stationIds
        ]
    ]

getAllStationsByCity ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Kernel.Types.Id.Id MerchantOperatingCity ->
  m [Domain.Types.StationsExtraInformation.StationsExtraInformation]
getAllStationsByCity city =
  findAllWithKV
    [ Se.Is Beam.merchantOperatingCityId $ Se.Eq city.getId
    ]
