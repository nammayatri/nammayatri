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

module Beckn.ACL.FRFS.Cancel (buildCancelReq) where

import qualified Beckn.ACL.FRFS.Utils as Utils
import qualified BecknV2.FRFS.Enums as Spec
import qualified BecknV2.FRFS.Types as Spec
import qualified BecknV2.FRFS.Utils as Utils
import Domain.Types.BecknConfig
import qualified Domain.Types.FRFSTicketBooking as DBooking
import Kernel.Prelude
import Kernel.Types.Beckn.Context as Context
import Kernel.Types.Error
import Kernel.Utils.Common

buildCancelReq ::
  (MonadFlow m) =>
  DBooking.FRFSTicketBooking ->
  BecknConfig ->
  Utils.BppData ->
  Spec.CancellationType ->
  Context.City ->
  m (Spec.CancelReq)
buildCancelReq booking bapConfig bppData cancellationType city = do
  now <- getCurrentTime
  let transactionId = booking.searchId.getId
      validTill = addUTCTime (intToNominalDiffTime 30) now
      ttl = diffUTCTime validTill now
  messageId <- generateGUID

  context <- Utils.buildContext Spec.CANCEL bapConfig transactionId messageId (Just $ Utils.durationToText ttl) (Just bppData) city

  bppOrderId <- booking.bppOrderId & fromMaybeM (InternalError "bppOrderId not found")
  pure $
    Spec.CancelReq
      { cancelReqContext = context,
        cancelReqMessage = tfCancelMessage bppOrderId cancellationType
      }

tfCancelMessage :: Text -> Spec.CancellationType -> Spec.CancelReqMessage
tfCancelMessage bppOrderId cancellationType =
  Spec.CancelReqMessage
    { cancelReqMessageCancellationReasonId = Just "7", -- TODO: Get details around this from ONDC
      cancelReqMessageDescriptor =
        Just $
          Spec.Descriptor
            { descriptorName = Just "Ride Cancellation",
              descriptorCode = Just $ show cancellationType,
              descriptorImages = Nothing
            },
      cancelReqMessageOrderId = bppOrderId
    }
