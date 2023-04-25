{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.Queries.AllocatorJob where

import Domain.Types.Timetable (Timetable)
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Lib.Scheduler.JobStorageType.DB.Queries (createJobIn, createManyJobs)
import Lib.Scheduler.Types
import SharedLogic.Allocator

createAllocatorSendSearchRequestToDriverJob :: NominalDiffTime -> Int -> SendSearchRequestToDriverJobData -> Esq.SqlDB ()
createAllocatorSendSearchRequestToDriverJob inTime maxShards jobData = do
  void $ createJobIn @_ @'SendSearchRequestToDriver inTime maxShards jobData

createUpcomingRideJobs :: Int -> [Id Timetable] -> Esq.SqlDB ()
createUpcomingRideJobs maxShards timetables =
  void $
    createManyJobs @_ @'AllocateDriverForUpcomingRide maxShards $
      fmap makeJobFromTimetable timetables
  where
    makeJobFromTimetable timetableId = do
      JobEntry
        { maxErrors = 3,
          jobData = AllocateDriverForUpcomingRideJobData timetableId
        }
