{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.Sos where

import Domain.Types.Person as Person ()
import Domain.Types.Sos as Sos
import qualified EulerHS.Language as L
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Lib.Utils
import qualified Sequelize as Se
import qualified Storage.Beam.Sos as BeamS

create :: (L.MonadFlow m, Log m) => Sos.Sos -> m ()
create = createWithKV

-- updateStatus :: Id Sos.Sos -> Sos.SosStatus -> SqlDB ()
-- updateStatus sosId status = do
--   now <- getCurrentTime
--   Esq.update $ \tbl -> do
--     set
--       tbl
--       [ SosUpdatedAt =. val now,
--         SosStatus =. val status
--       ]
--     where_ $ tbl ^. SosId ==. val (getId sosId)

updateStatus :: (L.MonadFlow m, MonadTime m, Log m) => Id Sos.Sos -> Sos.SosStatus -> m ()
updateStatus sosId status = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamS.status status,
      Se.Set BeamS.updatedAt now
    ]
    [Se.Is BeamS.id $ Se.Eq (getId sosId)]

-- findById :: Transactionable m => Id Sos.Sos -> m (Maybe Sos)
-- findById = Esq.findById

findById :: (L.MonadFlow m, Log m) => Id Sos.Sos -> m (Maybe Sos)
findById sosId = findOneWithKV [Se.Is BeamS.id $ Se.Eq (getId sosId)]

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
            updatedAt = updatedAt
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
        BeamS.updatedAt = updatedAt
      }
