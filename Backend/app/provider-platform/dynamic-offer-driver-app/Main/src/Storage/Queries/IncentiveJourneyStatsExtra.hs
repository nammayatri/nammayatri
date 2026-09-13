{-# OPTIONS_GHC -Wno-orphans #-}

-- | Driver-facing aliases over shared lib Extra queries (personId column).
module Storage.Queries.IncentiveJourneyStatsExtra
  ( upsertJourneyStats,
    findHistoryByDriverIdAndCreatedAtRange,
    findByDriverIdAndPeriodKey,
    mkLocalDayUtcBounds,
    findStatsByDriverJourneyAndPeriod,
    findStatsByDriverAndMilestonePeriod,
  )
where

import Data.Time (Day)
import qualified Domain.Types.Person as DP
import Kernel.Prelude
import Kernel.Types.Common (Seconds)
import Kernel.Types.Id
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourney as DIJ
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourneyMilestone as DIJM
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourneyStats as DIJS
import Lib.IncentiveJourney.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.IncentiveJourney.Storage.Queries.IncentiveJourneyStatsExtra as Lib
import Storage.Beam.IncentiveJourney ()

upsertJourneyStats ::
  (BeamFlow m r) =>
  DIJS.IncentiveJourneyStats ->
  m DIJS.IncentiveJourneyStats
upsertJourneyStats = Lib.upsertJourneyStats

findHistoryByDriverIdAndCreatedAtRange ::
  (BeamFlow m r) =>
  Id DP.Person ->
  UTCTime ->
  UTCTime ->
  Maybe Int ->
  Maybe Int ->
  m [DIJS.IncentiveJourneyStats]
findHistoryByDriverIdAndCreatedAtRange driverId =
  Lib.findHistoryByPersonIdAndCreatedAtRange (cast driverId)

findByDriverIdAndPeriodKey ::
  (BeamFlow m r) =>
  Id DP.Person ->
  Text ->
  m [DIJS.IncentiveJourneyStats]
findByDriverIdAndPeriodKey driverId = Lib.findByPersonIdAndPeriodKey (cast driverId)

mkLocalDayUtcBounds :: Day -> Seconds -> (UTCTime, UTCTime)
mkLocalDayUtcBounds = Lib.mkLocalDayUtcBounds

findStatsByDriverJourneyAndPeriod ::
  (BeamFlow m r) =>
  Id DP.Person ->
  Id DIJ.IncentiveJourney ->
  Text ->
  m [DIJS.IncentiveJourneyStats]
findStatsByDriverJourneyAndPeriod driverId = Lib.findStatsByPersonJourneyAndPeriod (cast driverId)

findStatsByDriverAndMilestonePeriod ::
  (BeamFlow m r) =>
  Id DP.Person ->
  Id DIJ.IncentiveJourney ->
  Id DIJM.IncentiveJourneyMilestone ->
  Text ->
  m (Maybe DIJS.IncentiveJourneyStats)
findStatsByDriverAndMilestonePeriod driverId = Lib.findStatsByPersonAndMilestonePeriod (cast driverId)
