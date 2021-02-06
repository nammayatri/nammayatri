module Services.Runner.Allocation where

-- import App.Types
-- import Beckn.Utils.Common
-- import EulerHS.Prelude
-- import qualified Services.Allocation as A

-- compose the allocation logic with Flow methods
-- sleep if x amount of rides been processed (have a configurable parameter)

-- runAllocation :: Flow Int
-- runAllocation = A.process handle
--   where
--     handle =
--       A.ServiceHandle
--         { getCurrentTime = getCurrTime,
--           getDriverSortMode = undefined,
--           getConfiguredNotificationTime = undefined,
--           getConfiguredAllocationTime = undefined,
--           getTopRidesToAllocate = undefined,
--           getDriverPool = undefined,
--           getCurrentNotification = undefined,
--           sendNotification = undefined,
--           addNotificationStatus = undefined,
--           updateNotificationStatus = undefined,
--           resetLastRejectionTime = undefined,
--           getAttemptedDrivers = undefined,
--           getDriversWithNotification = undefined,
--           getFirstDriverInTheQueue = undefined,
--           checkAvailability = undefined,
--           getDriverResponse = undefined,
--           assignDriver = undefined,
--           cancelRide = undefined,
--           cleanupRide = undefined
--         }
