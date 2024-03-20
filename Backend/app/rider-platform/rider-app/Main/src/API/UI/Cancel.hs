{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.UI.Cancel
  ( API,
    handler,
    CancelAPI,
    GetCancellationDuesDetailsAPI,
    getCancellationDuesDetails,
  )
where

import qualified Beckn.ACL.Cancel as ACL
import qualified Beckn.OnDemand.Utils.Common as Utils
import BecknV2.Utils
import qualified Domain.Action.UI.Cancel as DCancel
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.Person as Person
import Environment
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess (Success))
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import qualified SharedLogic.CallBPP as CallBPP
import Storage.Beam.SystemConfigs ()
import qualified Storage.CachedQueries.BecknConfig as QBC
import qualified Storage.Queries.Booking as QBooking
import Tools.Auth
import Tools.TransactionLogs
import TransactionLogs.Interface
import TransactionLogs.Interface.Types

type API =
  CancelAPI
    :<|> "rideBooking"
      :> Capture "rideBookingId" (Id SRB.Booking)
      :> "softCancel"
      :> TokenAuth
      :> Post '[JSON] APISuccess
    :<|> "dispute"
      :> "cancellationDues"
      :> TokenAuth
      :> Post '[JSON] APISuccess
    :<|> GetCancellationDuesDetailsAPI

type CancelAPI =
  "rideBooking"
    :> Capture "rideBookingId" (Id SRB.Booking)
    :> "cancel"
    :> TokenAuth
    :> ReqBody '[JSON] DCancel.CancelReq
    :> Post '[JSON] APISuccess

type GetCancellationDuesDetailsAPI =
  "getCancellationDuesDetails"
    :> TokenAuth
    :> Get '[JSON] DCancel.CancellationDuesDetailsRes

-------- Cancel Flow----------

handler :: FlowServer API
handler =
  cancel
    :<|> softCancel
    :<|> disputeCancellationDues
    :<|> getCancellationDuesDetails

cancel ::
  Id SRB.Booking ->
  (Id Person.Person, Id Merchant.Merchant) ->
  DCancel.CancelReq ->
  FlowHandler APISuccess
cancel bookingId (personId, merchantId) req =
  withFlowHandlerAPI . withPersonIdLogTag personId $ do
    dCancelRes <- DCancel.cancel bookingId (personId, merchantId) req
    cancelBecknReq <- ACL.buildCancelReqV2 dCancelRes
    fork "sending cancel, pushing ondc logs" do
      let kafkaLog = TransactionLog "cancel" $ Req cancelBecknReq.cancelReqContext (toJSON cancelBecknReq.cancelReqMessage)
      pushBecknLogToKafka kafkaLog
      let transactionLog = TransactionLogReq "cancel" $ ReqLog (toJSON cancelBecknReq.cancelReqContext) (maskSensitiveData $ toJSON cancelBecknReq.cancelReqMessage)
      booking <- QBooking.findById bookingId >>= fromMaybeM (BookingDoesNotExist bookingId.getId)
      becknConfig <- QBC.findByMerchantIdDomainAndVehicle booking.merchantId "MOBILITY" (Utils.mapVariantToVehicle booking.vehicleVariant) >>= fromMaybeM (InternalError "Beckn Config not found")
      void $ pushTxnLogs (ONDCCfg $ ONDCConfig {apiToken = becknConfig.logsToken, url = becknConfig.logsUrl}) transactionLog -- shrey00 : Maybe validate ONDC response?
    void $ withShortRetry $ CallBPP.cancelV2 dCancelRes.bppUrl cancelBecknReq
    return Success

softCancel ::
  Id SRB.Booking ->
  (Id Person.Person, Id Merchant.Merchant) ->
  FlowHandler APISuccess
softCancel bookingId (personId, merchantId) =
  withFlowHandlerAPI . withPersonIdLogTag personId $ do
    dCancelRes <- DCancel.softCancel bookingId (personId, merchantId)
    cancelBecknReq <- ACL.buildCancelReqV2 dCancelRes
    fork "sending cancel, pushing ondc logs" do
      let kafkaLog = TransactionLog "cancel" $ Req cancelBecknReq.cancelReqContext (toJSON cancelBecknReq.cancelReqMessage)
      pushBecknLogToKafka kafkaLog
      let transactionLog = TransactionLogReq "cancel" $ ReqLog (toJSON cancelBecknReq.cancelReqContext) (maskSensitiveData $ toJSON cancelBecknReq.cancelReqMessage)
      booking <- QBooking.findById bookingId >>= fromMaybeM (BookingDoesNotExist bookingId.getId)
      becknConfig <- QBC.findByMerchantIdDomainAndVehicle booking.merchantId "MOBILITY" (Utils.mapVariantToVehicle booking.vehicleVariant) >>= fromMaybeM (InternalError "Beckn Config not found")
      void $ pushTxnLogs (ONDCCfg $ ONDCConfig {apiToken = becknConfig.logsToken, url = becknConfig.logsUrl}) transactionLog -- shrey00 : Maybe validate ONDC response?
    void $ withShortRetry $ CallBPP.cancelV2 dCancelRes.bppUrl cancelBecknReq
    return Success

disputeCancellationDues :: (Id Person.Person, Id Merchant.Merchant) -> FlowHandler APISuccess
disputeCancellationDues = withFlowHandlerAPI . DCancel.disputeCancellationDues

getCancellationDuesDetails :: (Id Person.Person, Id Merchant.Merchant) -> FlowHandler DCancel.CancellationDuesDetailsRes
getCancellationDuesDetails = withFlowHandlerAPI . DCancel.getCancellationDuesDetails
