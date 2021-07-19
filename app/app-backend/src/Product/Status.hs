module Product.Status (onStatus) where

import App.Types
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Common
import qualified Beckn.Types.Core.API.Status as API
import Beckn.Types.Core.Ack
import Beckn.Types.Id
import Beckn.Utils.Error
import Beckn.Utils.Servant.SignatureAuth (SignatureAuthResult (..))
import EulerHS.Prelude
import qualified Storage.Queries.Ride as QRide
import Types.Error
import qualified Types.Storage.Organization as Organization
import Utils.Common
import qualified Utils.Notifications as Notify

onStatus ::
  SignatureAuthResult Organization.Organization ->
  API.OnStatusReq ->
  FlowHandler API.OnStatusRes
onStatus _org req = withFlowHandlerBecknAPI $
  withTransactionIdLogTag req $ do
    case req.contents of
      Right msg -> do
        let prodInstId = Id $ msg.order.id
            orderState = fromBeckn $ msg.order.state
        updateProductInstanceStatus prodInstId orderState
      Left err -> logTagError "on_status req" $ "on_status error: " <> show err
    return Ack
  where
    updateProductInstanceStatus prodInstId piStatus = do
      ride <- QRide.findByProductInstanceId prodInstId >>= fromMaybeM RideDoesNotExist
      -- TODO: ADD STATUS CHANGE VALIDATION
      DB.runSqlDBTransaction $
        QRide.updateStatus ride.id piStatus
      Notify.notifyOnStatusUpdate ride piStatus
