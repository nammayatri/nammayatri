module Product.Ride where

import App.Types
import Beckn.Types.Id
import EulerHS.Prelude hiding (id)
import qualified Types.API.Ride as API
import Types.Error
import qualified Types.Storage.Person as SPerson
import qualified Types.Storage.ProductInstance as SPI
import Utils.Common

getDriverLoc :: Id SPI.ProductInstance -> Id SPerson.Person -> FlowHandler API.GetDriverLocRes
getDriverLoc _rideId _personId =
  withFlowHandlerAPI $
    throwError $ InternalError "Function doesn't implement yet."
