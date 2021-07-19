module Product.Call where

import App.Types
import Beckn.Types.Core.API.Call
import Beckn.Types.Core.Ack
import Beckn.Types.Id
import EulerHS.Prelude
import ExternalAPI.Flow as ExternalAPI
import qualified Storage.Queries.Ride as QRide
import Types.Error
import qualified Types.Storage.Person as SP
import Utils.Common
import qualified Types.Storage.Ride as SRide

initiateCall :: Id SRide.Ride -> Id SP.Person -> FlowHandler CallRes
initiateCall rideId _ = withFlowHandlerAPI $ do
  ride <- QRide.findById rideId >>= fromMaybeM RideDoesNotExist
  ExternalAPI.initiateCall ride.productInstanceId
  return Ack
