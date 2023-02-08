module Core.ACL.Cancel where

import qualified Beckn.Types.Core.Taxi.API.Cancel as Cancel
import qualified Domain.Action.Beckn.Cancel as DCancel
import EulerHS.Prelude
import Kernel.Product.Validation.Context
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common

buildCancelReq ::
  (HasFlowEnv m r '["coreVersion" ::: Text]) =>
  Cancel.CancelReq ->
  m DCancel.CancelReq
buildCancelReq req = do
  validateContext Context.CANCEL req.context
  let bookingId = Id req.message.order_id
  return $
    DCancel.CancelReq
      { ..
      }
