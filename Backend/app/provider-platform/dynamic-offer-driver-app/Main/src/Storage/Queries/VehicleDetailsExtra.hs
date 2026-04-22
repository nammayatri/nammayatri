module Storage.Queries.VehicleDetailsExtra where

import qualified Domain.Types.MerchantOperatingCity
import Domain.Types.VehicleDetails
import Kernel.Beam.Functions
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import Sequelize as Se
import qualified Storage.Beam.VehicleDetails as BeamVD
import Storage.Queries.OrphanInstances.VehicleDetails ()

-- Extra code goes here --
findAllVehicleDetails ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  m [VehicleDetails]
findAllVehicleDetails = findAllWithKV [Se.Is BeamVD.id $ Se.Not $ Se.Eq ""]

findAllByCountryAndMerchantOperatingCityIdWithFallback ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Kernel.Types.Beckn.Context.Country ->
  Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity ->
  m [VehicleDetails]
findAllByCountryAndMerchantOperatingCityIdWithFallback country merchantOperatingCityId =
  findAllWithKV
    [ Se.Or
        [ Se.Is BeamVD.specificMerchantOperatingCityId $ Se.Eq (Just $ Kernel.Types.Id.getId merchantOperatingCityId),
          Se.And
            [ Se.Is BeamVD.specificMerchantOperatingCityId $ Se.Eq Nothing,
              Se.Or
                [ Se.Is BeamVD.specificCountry $ Se.Eq Nothing,
                  Se.Is BeamVD.specificCountry $ Se.Eq (Just country)
                ]
            ]
        ]
    ]
