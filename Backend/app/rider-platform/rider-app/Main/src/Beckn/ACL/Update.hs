{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE OverloadedLabels #-}

module Beckn.ACL.Update
  ( buildUpdateReq,
    UpdateBuildReq (..),
  )
where

import qualified Beckn.ACL.Common as Common
import qualified Beckn.Types.Core.Taxi.Update as Update
import qualified Beckn.Types.Core.Taxi.Update.UpdateEvent.PaymentCompletedEvent as PaymentCompletedU
import Control.Lens ((%~))
import qualified Data.Text as T
import qualified Domain.Types.Booking as DBooking
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant.MerchantPaymentMethod as DMPM
import qualified Domain.Types.Ride as DRide
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Beckn.ReqTypes
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common

data UpdateBuildReq = PaymentCompletedBuildReq
  { bppBookingId :: Id DBooking.BPPBooking,
    bppRideId :: Id DRide.BPPRide,
    paymentMethodInfo :: DMPM.PaymentMethodInfo,
    bppId :: Text,
    bppUrl :: BaseUrl,
    transactionId :: Text,
    merchant :: DM.Merchant,
    city :: Context.City -- Booking city, not merchant default city
  }

buildUpdateReq ::
  (MonadFlow m, HasFlowEnv m r '["nwAddress" ::: BaseUrl]) =>
  UpdateBuildReq ->
  m (BecknReq Update.UpdateMessage)
buildUpdateReq res = do
  messageId <- generateGUID
  bapUrl <- asks (.nwAddress) <&> #baseUrlPath %~ (<> "/" <> T.unpack res.merchant.id.getId)
  -- TODO :: Add request city, after multiple city support on gateway.
  context <- buildContext Context.MOBILITY Context.UPDATE messageId (Just res.transactionId) res.merchant.bapId bapUrl (Just res.bppId) (Just res.bppUrl) res.merchant.defaultCity res.merchant.country False
  pure $ BecknReq context $ mkUpdateMessage res

mkUpdateMessage ::
  UpdateBuildReq ->
  Update.UpdateMessage
mkUpdateMessage req@PaymentCompletedBuildReq {} = do
  Update.UpdateMessage $
    Update.PaymentCompleted
      PaymentCompletedU.PaymentCompletedEvent
        { id = req.bppBookingId.getId,
          update_target = "fulfillment.state.code,payment.status",
          payment =
            PaymentCompletedU.Payment
              { collected_by = Common.castDPaymentCollector req.paymentMethodInfo.collectedBy,
                _type = Common.castDPaymentType req.paymentMethodInfo.paymentType,
                instrument = Common.castDPaymentInstrument req.paymentMethodInfo.paymentInstrument,
                status = PaymentCompletedU.PAID
              },
          fulfillment =
            PaymentCompletedU.FulfillmentInfo
              { id = req.bppRideId.getId
              }
        }
