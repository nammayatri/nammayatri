module Domain.Action.UI.RideDetails where

import Domain.Types.RideDetails
import Kernel.External.Encryption
import Kernel.Prelude

getDriverNumber :: EncFlow m r => RideDetails -> m (Maybe Text)
getDriverNumber rideDetails = do
  decMobileNumber <- mapM decrypt rideDetails.driverNumber
  return $ rideDetails.driverCountryCode <> decMobileNumber
