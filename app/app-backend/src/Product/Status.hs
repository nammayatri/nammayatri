module Product.Status (onStatus) where

import App.Types
import qualified Beckn.Types.Core.API.Status as API
import Beckn.Types.Core.Ack
import Beckn.Utils.Error
import Beckn.Utils.Servant.SignatureAuth (SignatureAuthResult (..))
import EulerHS.Prelude
import qualified Types.Storage.Organization as Organization
import Utils.Common

onStatus ::
  SignatureAuthResult Organization.Organization ->
  API.OnStatusReq ->
  FlowHandler API.OnStatusRes
onStatus _org req = withFlowHandlerBecknAPI $
  withTransactionIdLogTag req $ do
    -- case req.contents of
    --   -- Right msg -> do
    --   --   let quoteId = Id $ msg.order.id
    --   --       orderState = fromBeckn $ msg.order.state
    --   --   updateRideStatus quoteId orderState
    --   -- Left err -> logTagError "on_status req" $ "on_status error: " <> show err
    return Ack

-- where
--   updateRideStatus quoteId rideStatus = do
--     ride <- QRide.findByQuoteId quoteId >>= fromMaybeM RideDoesNotExist
--     -- TODO: ADD STATUS CHANGE VALIDATION
--     DB.runSqlDBTransaction $
--       QRide.updateStatus ride.id rideStatus
--     Notify.notifyOnStatusUpdate ride rideStatus
