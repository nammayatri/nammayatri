{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.MgEligibilityExtra where

import qualified Domain.Types.MgEligibility as DMgEl
import qualified Domain.Types.Person as DP
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import qualified Sequelize as Se
import qualified Storage.Beam.MgEligibility as BeamMgEl
import Storage.Queries.OrphanInstances.MgEligibility ()

-- | One page of a driver's invitations, newest first. The ordering and the paging are done by the
-- database; the caller drops invitations whose campaign is no longer open, so a page can hold fewer
-- than `limit` rows.
findAllByDriverIdPaged :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Id DP.Person -> Int -> Int -> m [DMgEl.MgEligibility]
findAllByDriverIdPaged driverId limit offset =
  findAllWithOptionsKV
    [Se.Is BeamMgEl.driverId $ Se.Eq driverId.getId]
    (Se.Desc BeamMgEl.invitedAt)
    (Just limit)
    (Just offset)

-- | The invitations of a set of drivers, in one query. The caller passes one page of drivers, so the
-- list is small.
findAllByDriverIds :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => [Id DP.Person] -> m [DMgEl.MgEligibility]
findAllByDriverIds [] = pure []
findAllByDriverIds driverIds =
  findAllWithKV [Se.Is BeamMgEl.driverId $ Se.In (map (.getId) driverIds)]
