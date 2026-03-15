{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.CorporateRoute where

import qualified Domain.Types.CorporateRoute
import qualified Domain.Types.CorporateShift
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.CorporateRoute as Beam
import Storage.Queries.OrphanInstances.CorporateRoute ()

findById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Kernel.Types.Id.Id Domain.Types.CorporateRoute.CorporateRoute ->
  m (Maybe Domain.Types.CorporateRoute.CorporateRoute)
findById routeId = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId routeId)]

findByShiftId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Kernel.Types.Id.Id Domain.Types.CorporateShift.CorporateShift ->
  m [Domain.Types.CorporateRoute.CorporateRoute]
findByShiftId shiftId = do findAllWithKV [Se.Is Beam.shiftId $ Se.Eq (Kernel.Types.Id.getId shiftId)]

findActiveByShiftId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Kernel.Types.Id.Id Domain.Types.CorporateShift.CorporateShift ->
  m [Domain.Types.CorporateRoute.CorporateRoute]
findActiveByShiftId shiftId = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.shiftId $ Se.Eq (Kernel.Types.Id.getId shiftId),
          Se.Is Beam.status $ Se.Eq ("CR_ACTIVE" :: Text)
        ]
    ]
