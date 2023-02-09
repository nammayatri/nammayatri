module Beckn.ACL.Search (buildSearchReq) where

import qualified Beckn.Types.Core.Taxi.API.Search as Search
import qualified Beckn.Types.Core.Taxi.Search as Search
import qualified Domain.Action.Beckn.Search as DSearch
import EulerHS.Prelude hiding (state)
import Kernel.Product.Validation.Context
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common
import qualified Kernel.Types.Registry.Subscriber as Subscriber
import Kernel.Utils.Common
import Tools.Error

buildSearchReq ::
  (HasFlowEnv m r '["coreVersion" ::: Text]) =>
  Subscriber.Subscriber ->
  Search.SearchReq ->
  m DSearch.DSearchReq
buildSearchReq subscriber req = do
  let context = req.context
  validateContext Context.SEARCH context
  let intent = req.message.intent
  let pickup = intent.fulfillment.start
  let mbDropOff = intent.fulfillment.end
  unless (subscriber.subscriber_id == context.bap_id) $
    throwError (InvalidRequest "Invalid bap_id")
  unless (subscriber.subscriber_url == context.bap_uri) $
    throwError (InvalidRequest "Invalid bap_uri") -- is it correct?
  let messageId = context.message_id
  transactionId <- context.transaction_id & fromMaybeM (InvalidRequest "Missing transaction_id")
  pure
    DSearch.DSearchReq
      { messageId = messageId,
        transactionId = transactionId,
        bapId = subscriber.subscriber_id,
        bapUri = subscriber.subscriber_url,
        pickupLocation = mkLocation pickup.location,
        pickupTime = pickup.time.timestamp,
        mbDropLocation = mkLocation <$> (mbDropOff <&> (.location))
      }

mkLocation :: Search.Location -> DSearch.LocationReq
mkLocation (Search.Location Search.Gps {..}) =
  DSearch.LocationReq
    { ..
    }
