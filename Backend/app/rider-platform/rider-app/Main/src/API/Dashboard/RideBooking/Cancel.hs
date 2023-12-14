{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TemplateHaskell #-}

module API.Dashboard.RideBooking.Cancel where

import qualified Beckn.ACL.Cancel as ACL
import qualified Domain.Action.UI.Cancel as DCancel
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import Environment
import EulerHS.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import qualified SharedLogic.CallBPP as CallBPP
import SharedLogic.Merchant
import Storage.Beam.SystemConfigs ()

data RideCancelEndPoint = RideBookingCancelEndPoint
  deriving (Show, Read)

derivePersistField "RideCancelEndPoint"

type API =
  "rideBooking"
    :> CancelBookingAPI

type CancelBookingAPI =
  "cancel"
    :> Capture "rideBookingId" (Id SRB.Booking)
    :> Capture "customerId" (Id DP.Person)
    :> ReqBody '[JSON] DCancel.CancelReq
    :> Post '[JSON] APISuccess

handler :: ShortId DM.Merchant -> FlowServer API
handler = callBookingCancel

callBookingCancel :: ShortId DM.Merchant -> Id SRB.Booking -> Id DP.Person -> DCancel.CancelReq -> FlowHandler APISuccess
callBookingCancel merchantId bookingId personId req = withFlowHandlerAPI . withPersonIdLogTag personId $ do
  m <- findMerchantByShortId merchantId
  dCancelRes <- DCancel.cancel bookingId (personId, m.id) req
  isBecknSpecVersion2 <- asks (.isBecknSpecVersion2)
  if isBecknSpecVersion2
    then do
      void $ withShortRetry $ CallBPP.cancelV2 dCancelRes.bppUrl =<< ACL.buildCancelReqV2 dCancelRes
    else do
      void $ withShortRetry $ CallBPP.cancel dCancelRes.bppUrl =<< ACL.buildCancelReq dCancelRes
  return Success
