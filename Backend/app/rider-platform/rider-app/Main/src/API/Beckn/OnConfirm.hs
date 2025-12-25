{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Beckn.OnConfirm (API, handler) where

import qualified Beckn.ACL.OnConfirm as ACL
import qualified Beckn.OnDemand.Utils.Common as Utils
import qualified Beckn.Types.Core.Taxi.API.OnConfirm as OnConfirm
import qualified BecknV2.OnDemand.Utils.Common as Utils
import Data.Text as T
import qualified Domain.Action.Beckn.OnConfirm as DOnConfirm
import Domain.Types
import Environment
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Beckn.Ack
import Kernel.Types.Error
import Kernel.Utils.Common
import Kernel.Utils.Servant.SignatureAuth
import Storage.Beam.SystemConfigs ()
import qualified Storage.CachedQueries.ValueAddNP as CQVAN
import qualified Storage.Queries.Booking as QRB
import qualified Tools.Metrics as Metrics
import TransactionLogs.PushLogs

type API = OnConfirm.OnConfirmAPIV2

handler :: SignatureAuthResult -> FlowServer API
handler = onConfirm

onConfirm ::
  SignatureAuthResult ->
  OnConfirm.OnConfirmReqV2 ->
  FlowHandler AckResponse
onConfirm _ reqV2 = withFlowHandlerBecknAPI do
  transactionId <- Utils.getTransactionId reqV2.onConfirmReqContext
  Utils.withTransactionIdLogTag transactionId $ do
    bppSubscriberId <- Utils.getContextBppId reqV2.onConfirmReqContext
    isValueAddNP <- CQVAN.isValueAddNP bppSubscriberId
    mbDOnConfirmReq <- ACL.buildOnConfirmReqV2 reqV2 isValueAddNP
    whenJust mbDOnConfirmReq $ \onConfirmReq -> do
      let bppBookingId = case onConfirmReq of
            DOnConfirm.RideAssigned rideAssignedReq -> rideAssignedReq.bppBookingId
            DOnConfirm.BookingConfirmed bookingConfirmedReq -> bookingConfirmedReq.bppBookingId
      Redis.whenWithLockRedis (onConfirmLockKey bppBookingId.getId) 60 $ do
        validatedReq <- DOnConfirm.validateRequest onConfirmReq transactionId isValueAddNP
        merchantOperatingCityId <- case validatedReq of
          DOnConfirm.ValidatedRideAssigned rideAssignedReq -> pure $ rideAssignedReq.booking.merchantOperatingCityId.getId
          DOnConfirm.ValidatedBookingConfirmed bookingConfirmedReq -> pure $ bookingConfirmedReq.booking.merchantOperatingCityId.getId
        Metrics.finishMetricsBap Metrics.CONFIRM "" transactionId merchantOperatingCityId
        fork "on confirm received pushing ondc logs" do
          booking <- QRB.findByBPPBookingId bppBookingId >>= fromMaybeM (BookingDoesNotExist $ "BppBookingId:-" <> bppBookingId.getId)
          void $ pushLogs "on_confirm" (toJSON reqV2) booking.merchantId.getId "MOBILITY"
        runInForkWithCheck
          "onConfirm request processing"
          ( pure $ case validatedReq of
              DOnConfirm.ValidatedRideAssigned rideAssignedReq -> rideAssignedReq.booking.tripCategory /= Just (OneWay MeterRide)
              _ -> True
          )
          (Redis.whenWithLockRedis (onConfirmProcessingLockKey bppBookingId.getId) 60 $ DOnConfirm.onConfirm validatedReq)
    pure Ack
  where
    runInForkWithCheck message checkShouldFork action = do
      shouldFork <- checkShouldFork
      if shouldFork
        then do
          fork message action
        else action

onConfirmLockKey :: Text -> Text
onConfirmLockKey id = "Customer:OnConfirm:BppBookingId-" <> id

onConfirmProcessingLockKey :: Text -> Text
onConfirmProcessingLockKey id = "Customer:OnConfirm:Processing:BppBookingId-" <> id
