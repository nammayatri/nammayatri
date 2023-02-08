module Core.ACL.Status where

import Core.Context (buildContext)
import qualified Core.Spec.Common.Context as Context
import qualified Core.Spec.Status as Status
import qualified Domain.Action.UI.TriggerStatus as DStatus
import Kernel.Prelude
import Kernel.Types.Beckn.ReqTypes
import Kernel.Types.Id
import Kernel.Utils.Common

buildStatusReq ::
  ( MonadFlow m,
    MonadReader r m,
    HasField "selfURI" r BaseUrl,
    HasField "selfId" r Text
  ) =>
  DStatus.StatusRes ->
  m (BecknReq Status.StatusMessage)
buildStatusReq DStatus.StatusRes {..} = do
  bapId <- asks (.selfId)
  bapUri <- asks (.selfURI)
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
