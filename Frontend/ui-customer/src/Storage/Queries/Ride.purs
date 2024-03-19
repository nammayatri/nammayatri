module Storage.Queries.Ride where

import Prelude

findAllRides :: String
findAllRides = "SELECT * FROM Ride"

findActiveRide :: String
findActiveRide = "SELECT * FROM Ride WHERE status = 'INPROGRESS' OR status = 'NEW'"


