{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Internal.Ride where

import qualified AWS.S3 as S3
import qualified Data.Text as T
import Domain.Types.Ride
import Domain.Utils
import Environment
import qualified IssueManagement.Storage.Queries.MediaFile as QMF
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Error
import Storage.Beam.IssueManagement ()
import qualified Storage.CachedQueries.Merchant as QM
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.Ride as QRide
import Tools.Error

getDeliveryImage :: Id Ride -> Maybe Text -> Flow Text
getDeliveryImage rideId apiKey = do
  ride <- runInReplica $ QRide.findById rideId >>= fromMaybeM (RideNotFound rideId.getId)
  booking <- runInReplica $ QBooking.findById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
  merchant <- QM.findById booking.providerId >>= fromMaybeM (MerchantNotFound booking.providerId.getId)
  unless (Just merchant.internalApiKey == apiKey) $
    throwError $ AuthBlocked "Invalid BPP internal api key"
  latestDeliveryFileId <- ride.deliveryFileIds >>= safeLast & fromMaybeM DeliveryImageNotFound
  mediaFile <- QMF.findById latestDeliveryFileId >>= fromMaybeM (FileDoNotExist latestDeliveryFileId.getId)
  mediaFilePath <- mediaFile.s3FilePath & fromMaybeM (FileDoNotExist latestDeliveryFileId.getId)
  S3.get $ T.unpack mediaFilePath
