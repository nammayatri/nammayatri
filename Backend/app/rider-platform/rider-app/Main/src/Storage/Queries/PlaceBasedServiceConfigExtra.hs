module Storage.Queries.PlaceBasedServiceConfigExtra where

import qualified Domain.Types.MerchantServiceConfig
import qualified Domain.Types.PlaceBasedServiceConfig
import qualified Domain.Types.TicketPlace
import Kernel.Beam.Functions
import Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import qualified Sequelize as Se
import qualified Storage.Beam.PlaceBasedServiceConfig as Beam
import Storage.Queries.OrphanInstances.PlaceBasedServiceConfig ()

findByPlaceIdAndServiceName :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Domain.Types.MerchantServiceConfig.ServiceName -> m (Maybe Domain.Types.PlaceBasedServiceConfig.PlaceBasedServiceConfig)
findByPlaceIdAndServiceName (Kernel.Types.Id.Id placeId) serviceName = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.placeId $ Se.Eq placeId,
          Se.Is Beam.serviceName $ Se.Eq serviceName
        ]
    ]
