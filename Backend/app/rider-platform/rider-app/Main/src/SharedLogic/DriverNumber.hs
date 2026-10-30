module SharedLogic.DriverNumber
  ( getDriverMobileNumber,
    getDriverMobileNumberFromDecrypted,
  )
where

import qualified Domain.Types.Ride as DRide
import Kernel.External.Encryption
import Kernel.Prelude

getDriverMobileNumber :: EncFlow m r => DRide.Ride -> m (Maybe Text)
getDriverMobileNumber ride = case ride.driverPhoneNumber of
  Just encrypted -> Just <$> decrypt encrypted
  Nothing -> pure ride.driverMobileNumber

getDriverMobileNumberFromDecrypted :: DRide.DecryptedRide -> Maybe Text
getDriverMobileNumberFromDecrypted ride = maybe ride.driverMobileNumber Just ride.driverPhoneNumber
