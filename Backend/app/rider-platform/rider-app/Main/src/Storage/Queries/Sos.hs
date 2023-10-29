{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.Sos where

import Domain.Types.Person as Person ()
import qualified Domain.Types.Ride as SRide
import Domain.Types.Sos as Sos
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.Sos as BeamS

create :: MonadFlow m => Sos.Sos -> m ()
create = createWithKV

updateStatus :: MonadFlow m => Id Sos.Sos -> Sos.SosStatus -> m ()
updateStatus sosId status = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamS.status status,
      Se.Set BeamS.updatedAt now
    ]
    [Se.Is BeamS.id $ Se.Eq (getId sosId)]

findById :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Sos.Sos -> m (Maybe Sos)
findById sosId = findOneWithKV [Se.Is BeamS.id $ Se.Eq (getId sosId)]

findByRideIdAndStatus :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id SRide.Ride -> SosStatus -> m (Maybe Sos)
findByRideIdAndStatus rideId status =
  findOneWithKV
    [ Se.Is BeamS.rideId $ Se.Eq (getId rideId),
      Se.Is BeamS.status $ Se.Not $ Se.Eq status
    ]

instance FromTType' BeamS.Sos Sos where
  fromTType' BeamS.SosT {..} = do
    pure $
      Just
        Sos
          { id = Id id,
            personId = Id personId,
            rideId = Id rideId,
            status = status,
            flow = flow,
            createdAt = createdAt,
            updatedAt = updatedAt,
            ticketId = ticketId
          }

instance ToTType' BeamS.Sos Sos where
  toTType' Sos {..} = do
    BeamS.SosT
      { BeamS.id = getId id,
        BeamS.personId = getId personId,
        BeamS.rideId = getId rideId,
        BeamS.status = status,
        BeamS.flow = flow,
        BeamS.createdAt = createdAt,
        BeamS.updatedAt = updatedAt,
        BeamS.ticketId = ticketId
      }
