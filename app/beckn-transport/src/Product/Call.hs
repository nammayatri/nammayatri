module Product.Call where

import App.Types
import Beckn.Types.Core.API.Call
import Beckn.Types.Core.Ack
import Beckn.Types.Id
import EulerHS.Prelude
import ExternalAPI.Flow as ExternalAPI
import qualified Types.Storage.Person as SP
import qualified Types.Storage.Ride as SRide
import Utils.Common

initiateCall :: Id SRide.Ride -> Id SP.Person -> FlowHandler CallRes
initiateCall rideId _ = withFlowHandlerAPI $ do
  ExternalAPI.initiateCall rideId
  return Ack
