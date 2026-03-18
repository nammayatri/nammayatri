module SharedLogic.DriverMilestoneNotification where

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

data MilestoneType = BIRTHDAY | ANNIVERSARY
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data DriverMilestone = DriverMilestone
  { driverId :: Id SP.Person,
    milestoneType :: MilestoneType,
    milestoneDate :: Calendar.Day
  }
  deriving (Show, Generic)

-- | Find all drivers whose birthday or anniversary falls on the given day (month + day match)
findDriversWithMilestoneOnDay ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  Calendar.Day ->
  m [DTDPQ.DriverProfileQuestions]
findDriversWithMilestoneOnDay today = do
  let (_, todayMonth, todayDay) = Calendar.toGregorian today
  -- Find drivers with birthday today
  birthdayDrivers <- findAllWithKV
    [ Se.And
        [ Se.Is Beam.birthday $ Se.Not $ Se.Eq Nothing
        ]
    ]
  -- Filter in-memory for month/day match (since Beam doesn't easily support EXTRACT)
  let matchesDay profile =
        let matchesBirthday = case profile.birthday of
              Just bday ->
                let (_, bm, bd) = Calendar.toGregorian bday
                 in bm == todayMonth && bd == todayDay
              Nothing -> False
            matchesAnniversary = case profile.anniversary of
              Just aday ->
                let (_, am, ad) = Calendar.toGregorian aday
                 in am == todayMonth && ad == todayDay
              Nothing -> False
         in matchesBirthday || matchesAnniversary
  pure $ filter matchesDay birthdayDrivers

-- | Build milestone records from a driver profile for today
buildMilestones :: Calendar.Day -> DTDPQ.DriverProfileQuestions -> [DriverMilestone]
buildMilestones today profile =
  let (_, todayMonth, todayDay) = Calendar.toGregorian today
      birthdayMilestone = case profile.birthday of
        Just bday ->
          let (_, bm, bd) = Calendar.toGregorian bday
           in if bm == todayMonth && bd == todayDay
                then [DriverMilestone profile.driverId BIRTHDAY bday]
                else []
        Nothing -> []
      anniversaryMilestone = case profile.anniversary of
        Just aday ->
          let (_, am, ad) = Calendar.toGregorian aday
           in if am == todayMonth && ad == todayDay
                then [DriverMilestone profile.driverId ANNIVERSARY aday]
                else []
        Nothing -> []
   in birthdayMilestone <> anniversaryMilestone

-- | Process all milestones for today - finds drivers and returns their milestones
processDriverMilestones ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  m [DriverMilestone]
processDriverMilestones = do
  now <- getCurrentTime
  let today = utctDay now
  drivers <- findDriversWithMilestoneOnDay today
  logInfo $ "Found " <> show (length drivers) <> " drivers with milestones today"
  let milestones = concatMap (buildMilestones today) drivers
  logInfo $ "Total milestones to process: " <> show (length milestones)
  pure milestones
