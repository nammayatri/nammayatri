{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.AlertIncident where

import qualified Domain.Types.AlertIncident
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.AlertIncident as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.AlertIncident.AlertIncident -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.AlertIncident.AlertIncident] -> m ())
createMany = traverse_ create

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.AlertIncident.AlertIncident -> m (Maybe Domain.Types.AlertIncident.AlertIncident))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findFiringIncident ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Maybe Int -> Maybe Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Domain.Types.AlertIncident.IncidentStatus -> m [Domain.Types.AlertIncident.AlertIncident])
findFiringIncident limit offset description status = do
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is Beam.description $ Se.Eq description,
          Se.Is Beam.status $ Se.Eq status
        ]
    ]
    (Se.Desc Beam.firingTime)
    limit
    offset

findIncidentToResolve ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Maybe Int -> Maybe Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> m [Domain.Types.AlertIncident.AlertIncident])
findIncidentToResolve limit offset description resolvedTime = do
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is Beam.description $ Se.Eq description,
          Se.Is Beam.resolvedTime $ Se.Eq resolvedTime
        ]
    ]
    (Se.Desc Beam.firingTime)
    limit
    offset

updateToResolved ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Domain.Types.AlertIncident.IncidentStatus -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Types.Id.Id Domain.Types.AlertIncident.AlertIncident -> m ())
updateToResolved status resolvedTime downtimeSeconds id = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.status status,
      Se.Set Beam.resolvedTime resolvedTime,
      Se.Set Beam.downtimeSeconds downtimeSeconds,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.AlertIncident.AlertIncident -> m (Maybe Domain.Types.AlertIncident.AlertIncident))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.AlertIncident Domain.Types.AlertIncident.AlertIncident where
  fromTType' (Beam.AlertIncidentT {..}) = do
    pure $
      Just
        Domain.Types.AlertIncident.AlertIncident
          { alertGroup = alertGroup,
            alertName = alertName,
            createdAt = createdAt,
            description = description,
            downtimeSeconds = downtimeSeconds,
            externalURL = externalURL,
            firingTime = firingTime,
            id = Kernel.Types.Id.Id id,
            rawPayload = rawPayload,
            receiver = receiver,
            resolvedTime = resolvedTime,
            serviceName = serviceName,
            severity = severity,
            status = status,
            updatedAt = updatedAt
          }

instance ToTType' Beam.AlertIncident Domain.Types.AlertIncident.AlertIncident where
  toTType' (Domain.Types.AlertIncident.AlertIncident {..}) = do
    Beam.AlertIncidentT
      { Beam.alertGroup = alertGroup,
        Beam.alertName = alertName,
        Beam.createdAt = createdAt,
        Beam.description = description,
        Beam.downtimeSeconds = downtimeSeconds,
        Beam.externalURL = externalURL,
        Beam.firingTime = firingTime,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.rawPayload = rawPayload,
        Beam.receiver = receiver,
        Beam.resolvedTime = resolvedTime,
        Beam.serviceName = serviceName,
        Beam.severity = severity,
        Beam.status = status,
        Beam.updatedAt = updatedAt
      }
