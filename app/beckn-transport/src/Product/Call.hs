module Product.Call where

import App.Types
import Beckn.Types.Core.API.Call
import Beckn.Types.Core.Ack
import Beckn.Types.Id
import EulerHS.Prelude
import ExternalAPI.Flow as ExternalAPI
import qualified Storage.Queries.ProductInstance as PI
import Types.Error
import qualified Types.Storage.Person as SP
import Types.Storage.ProductInstance (ProductInstance)
import Utils.Common

initiateCall :: Id ProductInstance -> Id SP.Person -> FlowHandler CallRes
initiateCall rideId _ = withFlowHandlerAPI $ do
  prdInstance <- PI.findById rideId >>= fromMaybeM PIDoesNotExist -- RIDEORDER PI
  rideSearchProductInstanceId <- prdInstance.parentId & fromMaybeM (PIFieldNotPresent "parent_id")
  ExternalAPI.initiateCall rideSearchProductInstanceId -- RIDESEARCH PI
  return Ack
