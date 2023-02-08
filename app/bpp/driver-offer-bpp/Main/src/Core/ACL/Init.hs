module Core.ACL.Init where

import qualified Beckn.Types.Core.Taxi.API.Init as Init
import qualified Domain.Action.Beckn.Init as DInit
import Kernel.Prelude
import qualified Kernel.Product.Validation.Context as Context
import Kernel.Types.App
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Error
import Kernel.Types.Field
import Kernel.Types.Id
import qualified Kernel.Types.Registry.Subscriber as Subscriber
import Kernel.Utils.Error.Throwing

buildInitReq ::
  ( MonadThrow m,
    HasFlowEnv m r '["coreVersion" ::: Text]
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
