module Core.ACL.Init where

import Beckn.Prelude
import qualified Beckn.Product.Validation.Context as Context
import Beckn.Types.App
import qualified Beckn.Types.Core.Context as Context
import qualified Beckn.Types.Core.Taxi.API.Init as Init
import Beckn.Types.Error
import Beckn.Types.Field
import Beckn.Types.Id
import qualified Beckn.Types.Registry.Subscriber as Subscriber
import Beckn.Utils.Error.Throwing
import qualified Domain.Action.Beckn.Init as DInit

buildInitReq ::
  ( MonadThrow m,
    HasFlowEnv m r ["coreVersion" ::: Text, "domainVersion" ::: Text]
  ) =>
  Subscriber.Subscriber ->
  Init.InitReq ->
  m DInit.InitReq
buildInitReq subscriber req = do
  let context = req.context
  Context.validateContext Context.INIT context
  let order = req.message.order
  item <- case order.items of
    [it] -> pure it
    _ -> throwError $ InvalidRequest "There must be exactly one item in init request"
  itemId <- item.id & fromMaybeM (InvalidRequest "Item id required")
  -- should we check start time and other details?
  unless (subscriber.subscriber_id == context.bap_id) $
    throwError (InvalidRequest "Invalid bap_id")
  unless (subscriber.subscriber_url == context.bap_uri) $
    throwError (InvalidRequest "Invalid bap_uri")

  pure
    DInit.InitReq
      { driverQuoteId = Id itemId,
        bapId = subscriber.subscriber_id,
        bapUri = subscriber.subscriber_url
      }
