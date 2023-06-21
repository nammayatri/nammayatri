module Storage.Queries.Sos where

import Domain.Types.Person as Person ()
import Domain.Types.Sos as Sos
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Common
import Kernel.Types.Id
import qualified Storage.Beam.Sos as BeamS
import Storage.Tabular.Sos

create :: Sos.Sos -> SqlDB ()
create = Esq.create

updateStatus :: Id Sos.Sos -> Sos.SosStatus -> SqlDB ()
updateStatus sosId status = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ SosUpdatedAt =. val now,
        SosStatus =. val status
      ]
    where_ $ tbl ^. SosId ==. val (getId sosId)

findById :: Transactionable m => Id Sos.Sos -> m (Maybe Sos)
findById = Esq.findById

transformBeamSosToDomain :: BeamS.Sos -> Sos
transformBeamSosToDomain BeamS.SosT {..} = do
  Sos
    { id = Id id,
      personId = Id personId,
      rideId = Id rideId,
      status = status,
      flow = flow,
      createdAt = createdAt,
      updatedAt = updatedAt
    }

transformDomainSosToBeam :: Sos -> BeamS.Sos
transformDomainSosToBeam Sos {..} =
  BeamS.defaultSos
    { BeamS.id = getId id,
      BeamS.personId = getId personId,
      BeamS.rideId = getId rideId,
      BeamS.status = status,
      BeamS.flow = flow,
      BeamS.createdAt = createdAt,
      BeamS.updatedAt = updatedAt
    }
