module Flow.Lookup where

import App.Types (FlowHandler)
import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Core.Ack
import Beckn.Types.Registry.API (LookupRequest, LookupResponse)
import Beckn.Utils.Error (withFlowHandlerAPI)
import Domain.Subscriber
import Storage.Queries.Subscriber as Sub

lookup :: LookupRequest -> FlowHandler LookupResponse
lookup req = withFlowHandlerAPI $ do
  findByAll req.unique_key_id req.subscriber_id req.domain req._type

create :: Subscriber -> FlowHandler AckResponse
create sub = withFlowHandlerAPI $ do
  runTransaction $ Sub.create sub
  return Ack

delete :: Text -> Text -> FlowHandler AckResponse
delete uniqueKeyId subscriberId = withFlowHandlerAPI $ do
  runTransaction $ Sub.deleteByKey (uniqueKeyId, subscriberId)
  return Ack
