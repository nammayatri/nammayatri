module Product.Update where

import App.Types
import Beckn.Types.Common
import Beckn.Types.Core.API.Update
import Beckn.Types.Core.Ack
import Beckn.Types.Core.DecimalValue
import Beckn.Types.Core.Tracking
import Beckn.Types.Id
import Beckn.Types.Mobility.Trip
import Beckn.Utils.Servant.SignatureAuth (SignatureAuthResult (..))
import EulerHS.Prelude
import qualified Storage.Queries.Ride as QRide
import Types.Error
import qualified Types.ProductInfo as ProdInfo
import qualified Types.Storage.Organization as Organization
import Utils.Common

onUpdate ::
  SignatureAuthResult Organization.Organization ->
  OnUpdateReq ->
  FlowHandler AckResponse
onUpdate _org req = withFlowHandlerBecknAPI $
  withTransactionIdLogTag req $ do
    -- TODO: Verify api key here
    logTagInfo "on_update req" (show req)
    validateContext "on_update" $ req.context
    case req.contents of
      Right msg -> do
        let trip = msg.order.trip
            pid = Id $ msg.order.id
        ride <- QRide.findByProductInstanceId pid >>= fromMaybeM PIDoesNotExist
        let mprdInfo = decodeFromText =<< (ride.info)
            uInfo = getUpdatedProdInfo trip mprdInfo $ toBeckn <$> (ProdInfo.tracking =<< ProdInfo.tracker =<< mprdInfo)
            uRide =
              ride{info = encodeToText <$> uInfo,
                   actualPrice =
                     trip >>= fare >>= (.computed_value) >>= convertDecimalValueToAmount,
                   actualDistance =
                     trip >>= (.route) >>= (.edge.distance.computed_value)
                  }
        QRide.updateMultipleFlow (ride.id) uRide
      Left err -> logTagError "on_update req" $ "on_update error: " <> show err
    return Ack
  where
    getUpdatedProdInfo :: Maybe Trip -> Maybe ProdInfo.ProductInfo -> Maybe Tracking -> Maybe ProdInfo.ProductInfo
    getUpdatedProdInfo (Just trip) (Just prdInfo) mtracking =
      let utracker = ProdInfo.Tracker {trip = fromBeckn trip, tracking = fromBeckn <$> mtracking}
       in Just $ prdInfo {ProdInfo.tracker = Just utracker}
    getUpdatedProdInfo _ _ _ = Nothing
