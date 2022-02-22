module Core.ACL.Status where

import Beckn.Prelude
import Beckn.Types.Core.ReqTypes
import Beckn.Types.Id
import Beckn.Utils.Common
import Core.Context (buildContext)
import qualified Core.Spec.Common.Context as Context
import qualified Core.Spec.Status as Status
import qualified Domain.Action.UI.TriggerStatus as DStatus

buildStatusReq ::
  ( MonadFlow m,
    MonadReader r m,
    HasInConfig r c "selfURI" BaseUrl,
    HasInConfig r c "selfId" Text
  ) =>
  DStatus.StatusRes ->
  m (BecknReq Status.StatusMessage)
buildStatusReq DStatus.StatusRes {..} = do
  bapId <- asks (.config.selfId)
  bapUri <- asks (.config.selfURI)
  context <- buildContext Context.STATUS (getId bookingId) bapId bapUri (Just bppId) (Just bppUrl)
  pure $ BecknReq context statusMessage
  where
    statusMessage =
      Status.StatusMessage
        { order =
            Status.Order
              { id = ticketId
              }
        }
