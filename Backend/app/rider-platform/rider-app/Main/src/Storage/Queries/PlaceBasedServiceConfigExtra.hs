{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.PlaceBasedServiceConfigExtra where

import qualified Domain.Types.MerchantServiceConfig
import qualified Domain.Types.PlaceBasedServiceConfig
import qualified Domain.Types.TicketPlace
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (KvDbFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.PlaceBasedServiceConfig as Beam
import Storage.Queries.OrphanInstances.PlaceBasedServiceConfig

findByPlaceIdAndServiceName :: KvDbFlow m r => Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Domain.Types.MerchantServiceConfig.ServiceName -> m (Maybe (Domain.Types.PlaceBasedServiceConfig.PlaceBasedServiceConfig))
findByPlaceIdAndServiceName (Kernel.Types.Id.Id placeId) serviceName = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.placeId $ Se.Eq placeId,
          Se.Is Beam.serviceName $ Se.Eq serviceName
        ]
    ]
