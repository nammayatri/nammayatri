{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.UI.ExotelEndRide
  ( API,
    handler,
  )
where

import qualified Data.Text as T
-- import qualified Domain.Action.UI.ExotelEndRide as DExotelEndRide
import qualified Domain.Action.UI.Ride.EndRide as EndRide
import Environment
import Kernel.External.Encryption (getDbHash)
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Utils.Common
import Servant
import qualified Storage.CachedQueries.Exophone as CQExophone
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Ride as QRide
import Tools.Error

type API = CallBasedEndRideAPI

handler :: FlowServer API
handler = callBasedEndRidelHandler

-------- Direct call (Exotel) APIs
type CallBasedEndRideAPI =
  "exotel"
    :> "ride"
    :> ( "end"
           :> MandatoryQueryParam "CallFrom" Text
           :> MandatoryQueryParam "CallTo" Text
           :> Get '[JSON] AckResponse
       )

callBasedEndRidelHandler :: FlowServer CallBasedEndRideAPI
callBasedEndRidelHandler = callBasedEndRide

callBasedEndRide :: Text -> Text -> FlowHandler AckResponse
callBasedEndRide callFrom_ callTo_ = withFlowHandlerAPI $ do
  let callFrom = dropFirstZero callFrom_
  let callTo = dropFirstZero callTo_
  mobileNumberHash <- getDbHash callFrom
  exophone <- CQExophone.findByPhone callTo >>= fromMaybeM (ExophoneDoesNotExist callTo)
  driver <- Esq.runInReplica $ QPerson.findByMobileNumberAndMerchant "+91" mobileNumberHash exophone.merchantId >>= fromMaybeM (PersonWithPhoneNotFound callFrom)
  activeRide <- Esq.runInReplica $ QRide.getActiveByDriverId driver.id >>= fromMaybeM (RideForDriverNotFound driver.id.getId)
  --   -- _ <- runInReplica $ QRB.findById activeRide.bookingId >>= fromMaybeM (BookingNotFound $ getId activeRide.bookingId) -- not required, because we fetch booking in handler
  shandle <- EndRide.buildEndRideHandle exophone.merchantId activeRide.id -- TODO refactor to avoid extra queries to ride table
  -- DExotelEndRide.callBasedEndRide shandle exophone.merchantId mobileNumberHash callFrom
  _ <- EndRide.callBasedEndRide shandle activeRide.id (EndRide.CallBasedEndRideReq driver)
  return Ack
  where
    dropFirstZero = T.dropWhile (== '0')
