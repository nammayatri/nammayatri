module Storage.Queries.Ride where

import Prelude

findAllRides :: String
findAllRides = "SELECT * FROM Ride"

findActiveRide :: String
findActiveRide = "SELECT * FROM Ride WHERE status = 'TRIP_ASSIGNED' OR status = 'TRIP_STARTED'"

deleteActiveRide :: String -> String -- on the basis of userId and status
deleteActiveRide userId = "DELETE FROM Ride WHERE userId = '" <> userId <> "' AND (status = 'TRIP_ASSIGNED' OR status = 'TRIP_STARTED')"