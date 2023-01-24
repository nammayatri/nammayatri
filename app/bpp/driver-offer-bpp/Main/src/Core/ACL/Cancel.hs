module Core.ACL.Cancel where

import Beckn.Product.Validation.Context
import Beckn.Types.Common
import qualified Beckn.Types.Core.Context as Context
import qualified Beckn.Types.Core.Taxi.API.Cancel as Cancel
import Beckn.Types.Id
import Beckn.Utils.Common
import qualified Domain.Action.Beckn.Cancel as DCancel
import EulerHS.Prelude

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

buildCancelSearchReq ::
  (HasFlowEnv m r '["coreVersion" ::: Text]) =>
  Cancel.CancelReq ->
  m DCancel.CancelSearchReq
buildCancelSearchReq req = do
  validateContext Context.CANCEL req.context
  let searchId = Id req.message.item_id
  return $
    DCancel.CancelSearchReq
      { ..
      }