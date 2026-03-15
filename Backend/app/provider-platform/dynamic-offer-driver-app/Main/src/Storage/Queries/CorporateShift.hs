{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.CorporateShift where

import qualified Domain.Types.CorporateShift
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.CorporateShift as Beam
import Storage.Queries.OrphanInstances.CorporateShift ()

findById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Kernel.Types.Id.Id Domain.Types.CorporateShift.CorporateShift ->
  m (Maybe Domain.Types.CorporateShift.CorporateShift)
findById shiftId = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId shiftId)]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Kernel.Types.Id.Id Domain.Types.CorporateShift.CorporateShift ->
  m (Maybe Domain.Types.CorporateShift.CorporateShift)
findByPrimaryKey shiftId = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId shiftId)]]

-- | Find all active shifts. The driver-side CorporateShift type stores status as Text,
-- so we match against the string "CS_ACTIVE" which is the Show representation used on the rider side.
findAllActive ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  m [Domain.Types.CorporateShift.CorporateShift]
findAllActive = do
  findAllWithKV [Se.Is Beam.status $ Se.Eq ("CS_ACTIVE" :: Text)]
