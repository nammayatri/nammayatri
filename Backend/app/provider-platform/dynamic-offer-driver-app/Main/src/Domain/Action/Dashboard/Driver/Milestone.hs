module Domain.Action.Dashboard.Driver.Milestone
  ( getUpcomingMilestones,
    UpcomingMilestone (..),
    UpcomingMilestonesResp (..),
    MilestoneType (..),
  )
where

import qualified Data.Time.Calendar as Calendar
import qualified Domain.Types.DriverProfileQuestions as DTDPQ
import qualified Domain.Types.Person as SP
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.DriverProfileQuestions as Beam
import Storage.Queries.OrphanInstances.DriverProfileQuestions ()
import qualified Storage.Queries.Person as QPerson

data MilestoneType = BIRTHDAY | ANNIVERSARY
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

data UpcomingMilestone = UpcomingMilestone
  { driverId :: Id SP.Person,
    driverName :: Text,
    milestoneType :: MilestoneType,
    date :: Calendar.Day,
    daysFromNow :: Int
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

data UpcomingMilestonesResp = UpcomingMilestonesResp
  { milestones :: [UpcomingMilestone],
    totalCount :: Int
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

-- | Get drivers with upcoming milestones in the next N days
getUpcomingMilestones ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  Int ->
  Maybe MilestoneType ->
  m UpcomingMilestonesResp
getUpcomingMilestones daysAhead mbMilestoneType = do
  now <- getCurrentTime
  let today = utctDay now
  -- Fetch all profiles that have a birthday or anniversary set
  allProfiles <- findAllWithKV
    [ Se.Or
        [ Se.Is Beam.birthday $ Se.Not $ Se.Eq Nothing,
          Se.Is Beam.anniversary $ Se.Not $ Se.Eq Nothing
        ]
    ]
  -- Filter for upcoming milestones within the given window
  milestones <- fmap concat $ forM allProfiles $ \profile -> do
    let birthdayMilestones = case profile.birthday of
          Just bday -> milestoneIfUpcoming today daysAhead BIRTHDAY profile.driverId bday
          Nothing -> []
        anniversaryMilestones = case profile.anniversary of
          Just aday -> milestoneIfUpcoming today daysAhead ANNIVERSARY profile.driverId aday
          Nothing -> []
        allMilestones = birthdayMilestones <> anniversaryMilestones
        filtered = case mbMilestoneType of
          Just mt -> filter (\m -> m.milestoneType == mt) allMilestones
          Nothing -> allMilestones
    forM filtered $ \m -> do
      mbPerson <- QPerson.findById m.driverId
      let name = maybe "Unknown" (\p -> p.firstName <> maybe "" (" " <>) p.lastName) mbPerson
      pure m {driverName = name}
  pure $ UpcomingMilestonesResp milestones (length milestones)

milestoneIfUpcoming :: Calendar.Day -> Int -> MilestoneType -> Id SP.Person -> Calendar.Day -> [UpcomingMilestone]
milestoneIfUpcoming today daysAhead milestoneType driverId originalDate =
  let (todayYear, todayMonth, todayDay) = Calendar.toGregorian today
      (_, mMonth, mDay) = Calendar.toGregorian originalDate
      -- Build this year's occurrence
      thisYearDate = Calendar.fromGregorian todayYear mMonth mDay
      -- If it's already passed this year, check next year
      nextYearDate = Calendar.fromGregorian (todayYear + 1) mMonth mDay
      candidateDate = if thisYearDate >= today then thisYearDate else nextYearDate
      diff = fromIntegral $ Calendar.diffDays candidateDate today
   in if diff <= daysAhead
        then
          [ UpcomingMilestone
              { driverId = driverId,
                driverName = "", -- Will be filled in by caller
                milestoneType = milestoneType,
                date = candidateDate,
                daysFromNow = diff
              }
          ]
        else []
