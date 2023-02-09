module Beckn.ACL.OnConfirm (buildOnConfirmReq) where

import qualified Beckn.Types.Core.Taxi.API.OnConfirm as OnConfirm
import qualified Beckn.Types.Core.Taxi.OnConfirm as OnConfirm
import qualified Domain.Action.Beckn.OnConfirm as DOnConfirm
import Kernel.Prelude
import Kernel.Product.Validation.Context
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common

buildOnConfirmReq ::
  ( HasFlowEnv m r '["coreVersion" ::: Text]
  ) =>
  OnConfirm.OnConfirmReq ->
  m (Maybe DOnConfirm.OnConfirmReq)
buildOnConfirmReq req = do
  validateContext Context.ON_CONFIRM req.context
  handleError req.contents $ \message -> do
    return $
      DOnConfirm.OnConfirmReq
        { bppBookingId = Id message.order.id
        }

handleError ::
  (MonadFlow m) =>
  Either Error OnConfirm.OnConfirmMessage ->
  (OnConfirm.OnConfirmMessage -> m DOnConfirm.OnConfirmReq) ->
  m (Maybe DOnConfirm.OnConfirmReq)
handleError etr action =
  case etr of
    Right msg -> do
      Just <$> action msg
    Left err -> do
      logTagError "on_confirm req" $ "on_confirm error: " <> show err
      pure Nothing
