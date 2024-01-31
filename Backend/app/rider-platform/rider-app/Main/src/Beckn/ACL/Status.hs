{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE OverloadedLabels #-}

module Beckn.ACL.Status where

import qualified Beckn.Types.Core.Taxi.Status as Status
import qualified BecknV2.OnDemand.Types as Spec
import qualified BecknV2.OnDemand.Utils.Context as ContextV2
import Control.Lens ((%~))
import qualified Data.Text as T
import Domain.Types.Booking.Type (Booking)
import Domain.Types.Merchant (Merchant)
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Beckn.ReqTypes
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Utils.Common

data DStatusReq = DStatusReq
  { booking :: Booking,
    merchant :: Merchant,
    city :: Context.City
  }

buildStatusReq ::
  (HasFlowEnv m r '["nwAddress" ::: BaseUrl]) =>
  DStatusReq ->
  m (BecknReq Status.StatusMessage)
buildStatusReq DStatusReq {..} = do
  bppBookingId <- booking.bppBookingId & fromMaybeM (BookingFieldNotPresent "bppBookingId")
  messageId <- generateGUID
  bapUrl <- asks (.nwAddress) <&> #baseUrlPath %~ (<> "/" <> T.unpack merchant.id.getId)
  -- TODO :: Add request city, after multiple city support on gateway.
  context <-
    buildTaxiContext
      Context.STATUS
      messageId
      (Just booking.transactionId.getId)
      merchant.bapId
      bapUrl
      (Just merchant.id.getId)
      (Just booking.providerUrl)
      merchant.defaultCity
      merchant.country
      False
  pure $
    BecknReq context $
      Status.StatusMessage
        { order_id = bppBookingId.getId
        }

buildStatusReqV2 ::
  (HasFlowEnv m r '["nwAddress" ::: BaseUrl]) =>
  DStatusReq ->
  m Spec.StatusReq
buildStatusReqV2 DStatusReq {..} = do
  bppBookingId <- booking.bppBookingId & fromMaybeM (BookingFieldNotPresent "bppBookingId")
  messageId <- generateGUID
  bapUrl <- asks (.nwAddress) <&> #baseUrlPath %~ (<> "/" <> T.unpack merchant.id.getId)
  -- TODO :: Add request city, after multiple city support on gateway.
  context <-
    ContextV2.buildContextV2
      Context.STATUS
      Context.MOBILITY
      messageId
      (Just booking.transactionId.getId)
      merchant.bapId
      bapUrl
      (Just merchant.id.getId)
      (Just booking.providerUrl)
      merchant.defaultCity
      merchant.country

  pure $
    Spec.StatusReq
      { statusReqContext = context,
        statusReqMessage = tfMessage bppBookingId.getId
      }

tfMessage :: Text -> Spec.StatusReqMessage
tfMessage bppBookingId =
  Spec.StatusReqMessage
    { statusReqMessageRefId = Just bppBookingId
    }
