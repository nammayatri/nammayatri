{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.Status where

import Beckn.Context (buildContext)
import qualified Beckn.Spec.Common.Context as Context
import qualified Beckn.Spec.Status as Status
import qualified Domain.Action.UI.TriggerStatus as DStatus
import Kernel.Prelude
import Kernel.Types.Beckn.ReqTypes
import Kernel.Types.Id
import Kernel.Utils.Common hiding (buildContext)

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
