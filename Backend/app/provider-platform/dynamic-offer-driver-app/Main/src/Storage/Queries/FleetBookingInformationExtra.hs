{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FleetBookingInformationExtra where

import qualified Domain.Types.FleetBookingInformation
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FleetBookingInformation as Beam
import Storage.Queries.OrphanInstances.FleetBookingInformation

-- Extra code goes here --
findAllByFleetOwnerIdsAndFilters :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => [Text] -> Maybe Kernel.Prelude.UTCTime -> Maybe Kernel.Prelude.UTCTime -> Maybe Int -> Maybe Int -> m [Domain.Types.FleetBookingInformation.FleetBookingInformation]
findAllByFleetOwnerIdsAndFilters fleetOwnersIds from' to' limit offset =
  findAllWithOptionsKV
    [ Se.And $
        [ Se.Is Beam.fleetOwnerId $ Se.In $ map pure fleetOwnersIds
        ]
          <> ( case (from', to') of
                 (Just from, Just to) ->
                   [ Se.Is Beam.createdAt $ Se.GreaterThanOrEq from,
                     Se.Is Beam.createdAt $ Se.LessThan to
                   ]
                 (_, _) -> []
             )
    ]
    (Se.Desc Beam.createdAt)
    limit
    offset
