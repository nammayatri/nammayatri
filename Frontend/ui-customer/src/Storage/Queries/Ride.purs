module Storage.Queries.Ride where

import Prelude

findAllRides :: String
findAllRides = "SELECT * FROM Ride"

findActiveRide :: String
findActiveRide = "SELECT * FROM Ride WHERE status = 'TRIP_ASSIGNED' OR status = 'TRIP_STARTED'"


