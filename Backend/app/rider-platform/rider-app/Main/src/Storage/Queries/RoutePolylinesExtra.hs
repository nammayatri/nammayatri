{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.RoutePolylinesExtra where

import Domain.Types.MerchantOperatingCity
import Domain.Types.RoutePolylines
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.RoutePolylines as Beam
import Storage.Queries.OrphanInstances.RoutePolylines

-- Extra code goes here --

getByRouteIdsAndCity ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  [Kernel.Prelude.Text] ->
  Kernel.Types.Id.Id MerchantOperatingCity ->
  m [Domain.Types.RoutePolylines.RoutePolylines]
getByRouteIdsAndCity routeIds city =
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq city.getId,
          Se.Is Beam.routeId $ Se.In routeIds
        ]
    ]

getByRouteIdAndCity ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Text ->
  Id MerchantOperatingCity ->
  m (Maybe RoutePolylines)
getByRouteIdAndCity routeId city =
  findOneWithKV [Se.And [Se.Is Beam.routeId $ Se.Eq routeId, Se.Is Beam.merchantOperatingCityId $ Se.Eq city.getId]]
