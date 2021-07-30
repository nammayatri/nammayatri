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
import Utils.Common

initiateCall :: Id SP.Person -> CallReq -> FlowHandler CallRes
initiateCall _ req = withFlowHandlerAPI $ do
  prdInstance <- PI.findById (Id req.productInstanceId) >>= fromMaybeM PIDoesNotExist -- RIDEORDER PI
  Id rideSearchProductInstanceId <- prdInstance.parentId & fromMaybeM (PIFieldNotPresent "parent_id")
  ExternalAPI.initiateCall $ CallReq rideSearchProductInstanceId -- RIDESEARCH PI
  return Ack
