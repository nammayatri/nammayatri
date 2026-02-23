{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE OverloadedLabels #-}

module Beckn.ACL.FRFS.Cancel (buildCancelReq) where

import qualified Beckn.ACL.FRFS.Utils as Utils
import qualified BecknV2.FRFS.Enums as Spec
import qualified BecknV2.FRFS.Types as Spec
import qualified BecknV2.FRFS.Utils as Utils
import Control.Applicative ((<|>))
import Domain.Types.BecknConfig
import qualified Domain.Types.FRFSTicketBooking as DBooking
import Kernel.Prelude
import Kernel.Types.Beckn.Context as Context
import Kernel.Types.Error
import Kernel.Utils.Common

buildCancelReq ::
  (MonadFlow m, HasFlowEnv m r '["nwAddress" ::: BaseUrl]) =>
  Text ->
  DBooking.FRFSTicketBooking ->
  BecknConfig ->
  Utils.BppData ->
  Maybe Text ->
  Spec.CancellationType ->
  Context.City ->
  m (Spec.CancelReq)
buildCancelReq messageId booking bapConfig bppData mbCancellationReasonId cancellationType city = do
  now <- getCurrentTime
  let transactionId = booking.searchId.getId
      validTill = addUTCTime (intToNominalDiffTime (fromMaybe 30 bapConfig.cancelTTLSec)) now
      ttl = diffUTCTime validTill now

  context <- Utils.buildContext Spec.CANCEL bapConfig transactionId messageId (Just $ Utils.durationToText ttl) (Just bppData) city booking.vehicleType

  bppOrderId <- booking.bppOrderId & fromMaybeM (InternalError "bppOrderId not found")
  pure $
    Spec.CancelReq
      { cancelReqContext = context,
        cancelReqMessage = tfCancelMessage bppOrderId mbCancellationReasonId cancellationType
      }

tfCancelMessage :: Text -> Maybe Text -> Spec.CancellationType -> Spec.CancelReqMessage
tfCancelMessage bppOrderId cancellationReasonId cancellationType =
  Spec.CancelReqMessage
    { cancelReqMessageCancellationReasonId = cancellationReasonId <|> Just "7",
      cancelReqMessageDescriptor =
        Just $
          Spec.Descriptor
            { descriptorName = Just "Ride Cancellation",
              descriptorCode = Just $ show cancellationType,
              descriptorImages = Nothing
            },
      cancelReqMessageOrderId = bppOrderId
    }
