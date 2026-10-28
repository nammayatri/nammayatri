{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.MgEnrollmentExtra where

import Data.Time (Day)
import qualified Domain.Types.MgCampaign as DMgC
import qualified Domain.Types.MgEnrollment as DMgE
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import qualified Sequelize as Se
import qualified Storage.Beam.MgEnrollment as BeamME
import Storage.Queries.OrphanInstances.MgEnrollment ()

-- | Enrollments of a campaign still UPCOMING on or after `fromDay`: the days a cancelled campaign
-- has to call off. Days before `fromDay` are already over and are left for settlement.
findAllUpcomingFromDay :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Id DMgC.MgCampaign -> Day -> Int -> m [DMgE.MgEnrollment]
findAllUpcomingFromDay campaignId fromDay limit =
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is BeamME.campaignId $ Se.Eq campaignId.getId,
          Se.Is BeamME.status $ Se.Eq DMgE.UPCOMING,
          Se.Is BeamME.periodDate $ Se.GreaterThanOrEq fromDay
        ]
    ]
    (Se.Asc BeamME.periodDate)
    (Just limit)
    Nothing

-- | Enrollments of a campaign that still hold a budget reservation: every one that was not opted out
-- or called off. Rebuilds the reserved total when Redis has lost it.
findAllHoldingReservationByCampaignId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Id DMgC.MgCampaign -> m [DMgE.MgEnrollment]
findAllHoldingReservationByCampaignId campaignId =
  findAllWithKV
    [ Se.And
        [ Se.Is BeamME.campaignId $ Se.Eq campaignId.getId,
          Se.Is BeamME.status $ Se.In [DMgE.UPCOMING, DMgE.SETTLED, DMgE.DISQUALIFIED]
        ]
    ]

-- | Enrollments of a campaign that are still UPCOMING on or before `lastDueDay`, oldest day first.
-- The settlement job works out `lastDueDay` from the campaign's last window end and grace, so these
-- are exactly the days ready to settle. The range filter on periodDate runs against Postgres; a
-- row touched in the last moments before the read may not have reached it yet and is picked up by
-- the next run.
findAllUpcomingUpToDay :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Id DMgC.MgCampaign -> Day -> Int -> m [DMgE.MgEnrollment]
findAllUpcomingUpToDay campaignId lastDueDay limit =
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is BeamME.campaignId $ Se.Eq campaignId.getId,
          Se.Is BeamME.status $ Se.Eq DMgE.UPCOMING,
          Se.Is BeamME.periodDate $ Se.LessThanOrEq lastDueDay
        ]
    ]
    (Se.Asc BeamME.periodDate)
    (Just limit)
    Nothing
