{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.Booking
  ( BookingListRes (..),
    bookingStatus,
    bookingList,
    getBookingCancellationReason,
  )
where

import Beckn.Types.Core.Taxi.CancellationReasons.Types
import Data.OpenApi (ToSchema (..))
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.Person as Person
import EulerHS.Prelude hiding (id)
import Kernel.Storage.Esqueleto (runInReplica)
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.CancellationReasons
import Storage.CachedQueries.CacheConfig (CacheFlow)
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.Queries.Booking as QRB
import Tools.Error

newtype BookingListRes = BookingListRes
  { list :: [SRB.BookingAPIEntity]
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

bookingStatus :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) => Id SRB.Booking -> (Id Person.Person, Id Merchant.Merchant) -> m SRB.BookingAPIEntity
bookingStatus bookingId (personId, _) = do
  booking <- runInReplica (QRB.findById bookingId) >>= fromMaybeM (BookingDoesNotExist bookingId.getId)
  unless (booking.riderId == personId) $ throwError AccessDenied
  SRB.buildBookingAPIEntity booking

bookingList :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) => (Id Person.Person, Id Merchant.Merchant) -> Maybe Integer -> Maybe Integer -> Maybe Bool -> Maybe SRB.BookingStatus -> m BookingListRes
bookingList (personId, _) mbLimit mbOffset mbOnlyActive mbBookingStatus = do
  rbList <- runInReplica $ QRB.findAllByRiderIdAndRide personId mbLimit mbOffset mbOnlyActive mbBookingStatus
  BookingListRes <$> traverse SRB.buildBookingAPIEntity rbList

getBookingCancellationReason ::
  ( HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    CacheFlow m r,
    CoreMetrics m,
    EsqDBReplicaFlow m r,
    EsqDBFlow m r
  ) =>
  Id SRB.Booking ->
  m CancellationReasonsRes
getBookingCancellationReason bookingId = do
  booking <- runInReplica (QRB.findById bookingId) >>= fromMaybeM (BookingDoesNotExist bookingId.getId)
  merchant <- CQM.findById booking.merchantId >>= fromMaybeM (MerchantDoesNotExist booking.merchantId.getId)
  cancellationReasonsList <- getCancellationReasons booking.providerUrl booking.providerId merchant.city merchant.country booking.transactionId merchant.bapId
  buildCancellationReasonsRes booking.providerUrl booking.providerId merchant.city merchant.country booking.transactionId merchant.bapId cancellationReasonsList
