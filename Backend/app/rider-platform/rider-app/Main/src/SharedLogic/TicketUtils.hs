module SharedLogic.TicketUtils where

import qualified API.Types.Dashboard.AppManagement.TicketDashboard as API.Types.Dashboard.AppManagement.TicketDashboard
import Data.Ord as DO
import Data.Time hiding (getCurrentTime, secondsToNominalDiffTime)
import qualified Data.Time.Calendar as Data.Time.Calendar
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import qualified Domain.Types.Person as Domain.Types.Person
import qualified Domain.Types.ServiceCategory as Domain.Types.ServiceCategory
import qualified Environment as Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Types.Id as Kernel.Types.Id
import Kernel.Types.Time
import Kernel.Utils.Common
import qualified Storage.Queries.SeatManagement as QTSM
import qualified Storage.Queries.ServiceCategory as QSC
import Tools.Error

mkTicketServiceCategoryBlockedSeatKey :: Kernel.Types.Id.Id Domain.Types.ServiceCategory.ServiceCategory -> Data.Time.Calendar.Day -> Text
mkTicketServiceCategoryBlockedSeatKey categoryId visitDate = "TicketServiceCategory:blockedSet:id-" <> categoryId.getId <> "-date-" <> show visitDate

mkTicketServiceCategoryBookedCountKey :: Kernel.Types.Id.Id Domain.Types.ServiceCategory.ServiceCategory -> Data.Time.Calendar.Day -> Text
mkTicketServiceCategoryBookedCountKey categoryId visitDate = "TicketServiceCategory:bookedCount:id-" <> categoryId.getId <> "-date-" <> show visitDate

mkTicketServiceCategoryActiveBlockRequestKey :: Kernel.Types.Id.Id Domain.Types.ServiceCategory.ServiceCategory -> Data.Time.Calendar.Day -> Text
mkTicketServiceCategoryActiveBlockRequestKey categoryId visitDate = "TicketServiceCategory:activeBlockQuantityRequest:id-" <> categoryId.getId <> "-date-" <> show visitDate

mkTicketServiceCategoryConflictResolverKey :: Kernel.Types.Id.Id Domain.Types.ServiceCategory.ServiceCategory -> Data.Time.Calendar.Day -> Text
mkTicketServiceCategoryConflictResolverKey categoryId visitDate = "TicketServiceCategory:activeBlockConflictResolver:id-" <> categoryId.getId <> "-date-" <> show visitDate

mkTicketServiceAllowedMaxCapacityKey :: Kernel.Types.Id.Id Domain.Types.ServiceCategory.ServiceCategory -> Data.Time.Calendar.Day -> Text
mkTicketServiceAllowedMaxCapacityKey categoryId visitDate = "TicketServiceCategory:allowedMaxCapacity:id-" <> categoryId.getId <> "-date-" <> show visitDate

mkBlockMember :: Kernel.Types.Id.Id Domain.Types.Person.Person -> Int -> Text
mkBlockMember personId numOfUnits = "{" <> personId.getId <> "}:" <> show numOfUnits

releaseBlock :: Kernel.Types.Id.Id Domain.Types.Person.Person -> Data.Time.Calendar.Day -> [(Kernel.Types.Id.Id Domain.Types.ServiceCategory.ServiceCategory, Int)] -> Environment.Flow ()
releaseBlock personId visitDate = mapM_ (\(categoryId, categoryUnit) -> Redis.zRem (mkTicketServiceCategoryBlockedSeatKey categoryId visitDate) ([mkBlockMember personId categoryUnit]))

setupBlockMechanismNx :: Kernel.Types.Id.Id Domain.Types.ServiceCategory.ServiceCategory -> Data.Time.Calendar.Day -> Environment.Flow ()
setupBlockMechanismNx serviceCategoryId visitDate = do
  mbBookedCount :: Maybe Int <- Redis.get (mkTicketServiceCategoryBookedCountKey serviceCategoryId visitDate)
  mbAllowedMaxCapacity :: Maybe Int <- Redis.get (mkTicketServiceAllowedMaxCapacityKey serviceCategoryId visitDate)
  when (isNothing mbBookedCount || isNothing mbAllowedMaxCapacity) $ do
    serviceCategory <- QSC.findById serviceCategoryId >>= fromMaybeM (InternalError $ "Setup failed: Service Category id " <> serviceCategoryId.getId <> " not found")
    whenJust serviceCategory.availableSeats $ \maxSeats -> do
      mbSeatM <- QTSM.findByTicketServiceCategoryIdAndDate serviceCategoryId visitDate
      let alreadyBookedCount = maybe 0 (.booked) mbSeatM
      let allowedMaxCapacity = fromMaybe maxSeats (mbSeatM >>= (.maxCapacity))
      keyExpiryTime <- expirationTimeInSeconds visitDate
      let keysAndValues =
            [ (mkTicketServiceAllowedMaxCapacityKey serviceCategoryId visitDate, allowedMaxCapacity),
              (mkTicketServiceCategoryConflictResolverKey serviceCategoryId visitDate, 0),
              (mkTicketServiceCategoryActiveBlockRequestKey serviceCategoryId visitDate, 0),
              (mkTicketServiceCategoryBookedCountKey serviceCategoryId visitDate, alreadyBookedCount)
            ]
      when (keyExpiryTime > 0) $ forM_ keysAndValues $ \(key, value) -> void $ Redis.setNxExpire key keyExpiryTime value
  where
    endOfDayTime :: Day -> UTCTime
    endOfDayTime day = UTCTime day (secondsToDiffTime (24 * 60 * 60 - 1))

    expirationTimeInSeconds :: Day -> Environment.Flow Int
    expirationTimeInSeconds day = do
      currentTime <- getCurrentTime
      let endTime = endOfDayTime day
      return . round $ utcTimeToPOSIXSeconds endTime - utcTimeToPOSIXSeconds currentTime

seatStatus :: API.Types.Dashboard.AppManagement.TicketDashboard.CurrentSeatStatusReq -> Environment.Flow API.Types.Dashboard.AppManagement.TicketDashboard.CurrentSeatStatusResp
seatStatus req = do
  let serviceCategoryId = Kernel.Types.Id.Id req.serviceCategory
  let visitDate = req.date
  setupBlockMechanismNx serviceCategoryId visitDate
  mbBookedCount :: Maybe Int <- Redis.get (mkTicketServiceCategoryBookedCountKey serviceCategoryId visitDate)
  mbAllowedMaxCapacity :: Maybe Int <- Redis.get (mkTicketServiceAllowedMaxCapacityKey serviceCategoryId visitDate)
  if (isNothing mbBookedCount && isNothing mbAllowedMaxCapacity)
    then do
      mbSeatM <- QTSM.findByTicketServiceCategoryIdAndDate serviceCategoryId visitDate
      let bookedCount = maybe 0 (.booked) mbSeatM
      return $ API.Types.Dashboard.AppManagement.TicketDashboard.CurrentSeatStatusResp {maxCapacity = Nothing, remainingCapacity = Nothing, bookedCount = bookedCount, unlimitedCapacity = True}
    else do
      bookedCount <- mbBookedCount & fromMaybeM (InternalError "Booked count not found")
      maxCapacity <- mbAllowedMaxCapacity & fromMaybeM (InternalError "Max capacity not found")
      return $ API.Types.Dashboard.AppManagement.TicketDashboard.CurrentSeatStatusResp {maxCapacity = Just maxCapacity, remainingCapacity = Just (maxCapacity - bookedCount), bookedCount = bookedCount, unlimitedCapacity = False}
