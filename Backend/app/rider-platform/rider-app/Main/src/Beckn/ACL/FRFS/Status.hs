{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Beckn.ACL.FRFS.Status (buildStatusReq) where

import qualified Beckn.ACL.FRFS.Utils as Utils
import qualified BecknV2.FRFS.Enums as Spec
import qualified BecknV2.FRFS.Types as Spec
import Domain.Types.BecknConfig
import qualified Domain.Types.FRFSTicketBooking as DBooking
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common

buildStatusReq ::
  (MonadFlow m) =>
  DBooking.FRFSTicketBooking ->
  BecknConfig ->
  m (Spec.StatusReq)
buildStatusReq booking bapConfig = do
  let transactionId = booking.searchId.getId
  messageId <- generateGUID

  context <- Utils.buildContext Spec.INIT bapConfig transactionId messageId Nothing

  bppOrderId <- booking.bppOrderId & fromMaybeM (InternalError "bppOrderId not found")
  pure $
    Spec.StatusReq
      { statusReqContext = context,
        statusReqMessage = tfStatusMessage bppOrderId
      }

tfStatusMessage :: Text -> Spec.StatusReqMessage
tfStatusMessage bppOrderId =
  Spec.StatusReqMessage
    { statusReqMessageOrderId = bppOrderId
    }
