module Core.ACL.Search where

import Beckn.Prelude
import Beckn.Product.Validation.Context
import qualified Beckn.Types.Core.Context as Context
import qualified Beckn.Types.Core.Taxi.API.Search as Search
import qualified Beckn.Types.Core.Taxi.Search as Search
import qualified Beckn.Types.Registry.Subscriber as Subscriber
import Data.Maybe
import qualified Domain.Action.Beckn.Search as DSearch
import qualified Domain.Types.SearchReqLocation as Location
import Types.Error
import Utils.Common

buildSearchReq ::
  (HasFlowEnv m r ["coreVersion" ::: Text, "domainVersion" ::: Text]) =>
  Subscriber.Subscriber ->
  Subscriber.Subscriber ->
  Search.SearchReq ->
  m DSearch.DSearchReq
buildSearchReq subscriber gateway req = do
  let context = req.context
  validateContext Context.SEARCH context
  let intent = req.message.intent
  let pickup = intent.fulfillment.start
  let dropOff = fromJust intent.fulfillment.end
  unless (subscriber.subscriber_id == context.bap_id) $
    throwError (InvalidRequest "Invalid bap_id")
  unless (subscriber.subscriber_url == context.bap_uri) $
    throwError (InvalidRequest "Invalid bap_uri")
  let messageId = context.message_id
  pure
    DSearch.DSearchReq
      { messageId = messageId,
        transactionId = context.transaction_id,
        bapId = subscriber.subscriber_id,
        bapUri = subscriber.subscriber_url,
        gatewayUri = gateway.subscriber_url,
        pickupLocation = mkLocation pickup.location,
        pickupTime = pickup.time.timestamp,
        dropLocation = mkLocation dropOff.location
      }

mkLocation :: Search.Location -> Location.SearchReqLocationAPIEntity
mkLocation (Search.Location Search.Gps {..} Search.Address {..}) =
  Location.SearchReqLocationAPIEntity
    { areaCode = area_code,
      ..
    }
