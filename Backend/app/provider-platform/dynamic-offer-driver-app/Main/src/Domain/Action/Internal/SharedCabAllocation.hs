{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Internal.SharedCabAllocation where

import Data.OpenApi (ToSchema)
import Domain.Types.Booking as DBooking
import Domain.Types.Person as DP
import Environment
import EulerHS.Prelude
import Kernel.Beam.Functions
import Kernel.External.Types (Language (..))
import Kernel.Types.APISuccess
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.Merchant as QM
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.QueriesExtra.BookingLite as QBookingLite
import Tools.Notifications

data SharedCabAllocationReq = SharedCabAllocationReq
  { bookingId :: Id DBooking.Booking,
    driverId :: Id DP.Person,
    seats :: Maybe Int,
    boardingCode :: Maybe Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

sharedCabAllocationFCM :: SharedCabAllocationReq -> Maybe Text -> Flow APISuccess
sharedCabAllocationFCM req apiKey = do
  person <- runInReplica $ QPerson.findById req.driverId >>= fromMaybeM (PersonNotFound req.driverId.getId)
  merchant <- QM.findById person.merchantId >>= fromMaybeM (MerchantNotFound person.merchantId.getId)
  unless (Just merchant.internalApiKey == apiKey) $
    throwError $ AuthBlocked "Invalid BPP internal api key"
  _booking <- runInReplica $ QBookingLite.findByIdLite req.bookingId >>= fromMaybeM (BookingNotFound req.bookingId.getId)
  let entityData =
        SharedCabAllocationEntityData
          { bookingId = req.bookingId.getId,
            driverId = req.driverId.getId,
            seats = req.seats,
            boardingCode = req.boardingCode
          }
  notifySharedCabAllocation person.merchantOperatingCityId person.id person.deviceToken (fromMaybe ENGLISH person.language) entityData
  pure Success
