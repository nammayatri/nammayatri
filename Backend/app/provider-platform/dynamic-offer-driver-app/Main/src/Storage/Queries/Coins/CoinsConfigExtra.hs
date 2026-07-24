module Storage.Queries.Coins.CoinsConfigExtra where

import Domain.Types.Coins.CoinsConfig
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Domain.Types.VehicleCategory as DTV
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.DriverCoins.Types as DCT
import qualified Sequelize as Se
import qualified Storage.Beam.Coins.CoinsConfig as BeamDC
import Storage.Queries.Coins.CoinsConfig ()

findAllByMerchantOptCityIdWithLimitOffset ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  Maybe Text ->
  Maybe DTV.VehicleCategory ->
  Maybe Int ->
  Maybe Int ->
  m [CoinsConfig]
findAllByMerchantOptCityIdWithLimitOffset (Id merchantOptCityId) mbEventName mbVehicleCategory limit offset =
  findAllWithOptionsKV
    [ Se.And $
        [Se.Is BeamDC.merchantOptCityId $ Se.Eq merchantOptCityId]
          <> [Se.Is BeamDC.eventName $ Se.Eq eventName | Just eventName <- [mbEventName]]
          <> [Se.Is BeamDC.vehicleCategory $ Se.Eq (Just vehicleCategory) | Just vehicleCategory <- [mbVehicleCategory]]
    ]
    (Se.Asc BeamDC.id)
    limit
    offset

-- | Active EndRide coin configs for city + vehicle + trip category (used by incentive cohort window checks).
findActiveConfigsByCityVehicleTrip ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  Maybe DTV.VehicleCategory ->
  Maybe DCT.TripCategoryType ->
  m [CoinsConfig]
findActiveConfigsByCityVehicleTrip (Id merchantOptCityId) mbVehicleCategory mbTripCategoryType =
  findAllWithKV
    [ Se.And $
        [ Se.Is BeamDC.merchantOptCityId $ Se.Eq merchantOptCityId,
          Se.Is BeamDC.active $ Se.Eq True,
          Se.Is BeamDC.eventName $ Se.Eq "EndRide"
        ]
          <> [Se.Is BeamDC.vehicleCategory $ Se.Eq mbVehicleCategory]
          <> [Se.Is BeamDC.tripCategoryType $ Se.Eq mbTripCategoryType]
    ]
