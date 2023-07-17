{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.SyncBookingStatus
  ( syncBookingStatus,
    SyncBookingStatusReq,
  )
where

import Beckn.ACL.Status as ACL
import Domain.Types.Booking as DRB
import Domain.Types.Merchant
import qualified Domain.Types.Person as SP
import Environment
import qualified Kernel.Beam.Functions as B
import Kernel.Prelude
import Kernel.Types.APISuccess
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.CallBPP as CallBPP
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.Queries.Booking as QRB
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC


newtype SyncBookingStatusReq = SyncBookingStatusReq
  { bookingId :: Id Booking
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

syncBookingStatus :: Id SP.Person -> Id Merchant -> SyncBookingStatusReq -> Flow APISuccess
syncBookingStatus _ merchantId req = do
  merchant <- CQM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  now <- getCurrentTime
  booking <- B.runInReplica $ QRB.findById req.bookingId >>= fromMaybeM (BookingDoesNotExist req.bookingId.getId)
  city <-
    CQMOC.findById booking.merchantOperatingCityId
      >>= fmap (.city) . fromMaybeM (MerchantOperatingCityNotFound booking.merchantOperatingCityId.getId)
  case booking.bookingDetails of
    DRB.OneWaySpecialZoneDetails special_zone_details -> when (Just now < special_zone_details.otpValidTill) $ throwError SpecialZoneOtpValidTillNotYetExpired
    _ -> pure ()
  unless (booking.status == DRB.CONFIRMED) $ throwError (BookingInvalidStatus $ show booking.status)
  buildDStatusReq <- mkStatusReq booking merchant city
  becknStatusReq <- buildStatusReq buildDStatusReq
  void $ withShortRetry $ CallBPP.callStatus booking.providerUrl becknStatusReq
  pure Success
  where
    mkStatusReq booking merchant city = do
      return $
        ACL.DStatusReq
          { booking = booking,
            merchant = merchant,
            city = city
          }
