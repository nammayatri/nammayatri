{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.Booking where

import qualified Beckn.ACL.Status as StatusACL
import qualified Beckn.ACL.Update as ACL
import qualified Beckn.OnDemand.Utils.Common as Utils
import BecknV2.Utils
import Data.OpenApi (ToSchema (..))
import qualified Data.Time as DT
import qualified Domain.Action.UI.Cancel as DCancel
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.Booking.API as SRB
import Domain.Types.CancellationReason
import qualified Domain.Types.Client as DC
import Domain.Types.Location
import Domain.Types.LocationAddress
import qualified Domain.Types.LocationMapping as DLM
import Domain.Types.Merchant
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as Person
import qualified Domain.Types.VehicleServiceTier as DVST
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Beam.Functions
import Kernel.External.Maps (LatLong)
import Kernel.Prelude (intToNominalDiffTime)
import Kernel.Types.APISuccess (APISuccess (Success))
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.CallBPP as CallBPP
import qualified SharedLogic.Cancel as SHCancel
import qualified Storage.CachedQueries.BecknConfig as QBC
import qualified Storage.CachedQueries.Merchant as CQMerchant
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Merchant.RiderConfig as QRC
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.Location as QL
import qualified Storage.Queries.LocationMapping as QLM
import qualified Storage.Queries.Ride as QR
import Tools.Error

data StopReq = StopReq
  { gps :: LatLong,
    address :: LocationAddress
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

newtype BookingListRes = BookingListRes
  { list :: [SRB.BookingAPIEntity]
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

bookingStatus :: Id SRB.Booking -> (Id Person.Person, Id Merchant.Merchant) -> Flow SRB.BookingAPIEntity
bookingStatus bookingId _ = do
  booking <- runInReplica (QRB.findById bookingId) >>= fromMaybeM (BookingDoesNotExist bookingId.getId)
  logInfo $ "booking: test " <> show booking
  bapConfig <- QBC.findByMerchantIdDomainAndVehicle booking.merchantId "MOBILITY" (Utils.mapVariantToVehicle $ DVST.castServiceTierToVariant booking.vehicleServiceTierType) >>= fromMaybeM (InternalError "Beckn Config not found")
  confirmBufferTtl <- bapConfig.confirmBufferTTLSec & fromMaybeM (InternalError "Invalid ttl")
  now <- getCurrentTime
  confirmTtl <- bapConfig.confirmTTLSec & fromMaybeM (InternalError "Invalid ttl")
  initTtl <- bapConfig.initTTLSec & fromMaybeM (InternalError "Invalid ttl")
  let ttlInInt = initTtl + confirmTtl + confirmBufferTtl
      ttlToNominalDiffTime = intToNominalDiffTime ttlInInt
      ttlUtcTime = addDurationToUTCTime booking.createdAt ttlToNominalDiffTime
  when (booking.status == SRB.NEW && (ttlUtcTime < now)) do
    SHCancel.cancelHandler booking.id (booking.riderId, booking.merchantId) cancelReq
    throwError $ RideInvalidStatus "Booking Invalid"
  SRB.buildBookingAPIEntity booking booking.riderId
  where
    cancelReq =
      DCancel.CancelReq
        { reasonCode = CancellationReasonCode "External/Beckn API failure",
          reasonStage = OnConfirm,
          additionalInfo = Nothing
        }

checkBookingsForStatus :: [SRB.Booking] -> Flow ()
checkBookingsForStatus (currBooking : bookings) = do
  riderConfig <- QRC.findByMerchantOperatingCityId currBooking.merchantOperatingCityId >>= fromMaybeM (RiderConfigDoesNotExist currBooking.merchantOperatingCityId.getId)
  case (riderConfig.bookingSyncStatusCallSecondsDiffThreshold, currBooking.estimatedDuration) of
    (Just timeDiffThreshold, Just estimatedEndDuration) -> do
      now <- getCurrentTime
      let estimatedEndTime = DT.addUTCTime (fromIntegral estimatedEndDuration.getSeconds) currBooking.createdAt
      let diff = DT.diffUTCTime now estimatedEndTime
      let callStatusCondition = currBooking.status /= SRB.CANCELLED && currBooking.status /= SRB.COMPLETED && diff > fromIntegral timeDiffThreshold
      when callStatusCondition $ do
        merchant <- CQMerchant.findById currBooking.merchantId >>= fromMaybeM (MerchantNotFound currBooking.merchantId.getId)
        city <- CQMOC.findById currBooking.merchantOperatingCityId >>= fmap (.city) . fromMaybeM (MerchantOperatingCityNotFound currBooking.merchantOperatingCityId.getId)
        let dStatusReq = StatusACL.DStatusReq currBooking merchant city
        becknStatusReq <- StatusACL.buildStatusReqV2 dStatusReq
        void $ withShortRetry $ CallBPP.callStatusV2 currBooking.providerUrl becknStatusReq merchant.id
        checkBookingsForStatus bookings
    (_, _) -> logError "Nothing values for time diff threshold or booking end duration"
checkBookingsForStatus [] = pure ()

bookingList :: (Id Person.Person, Id Merchant.Merchant) -> Maybe Integer -> Maybe Integer -> Maybe Bool -> Maybe SRB.BookingStatus -> Maybe (Id DC.Client) -> Flow BookingListRes
bookingList (personId, _) mbLimit mbOffset mbOnlyActive mbBookingStatus mbClientId = do
  rbList <- runInReplica $ QR.findAllByRiderIdAndRide personId mbLimit mbOffset mbOnlyActive mbBookingStatus mbClientId
  fork "booking list status update" $ checkBookingsForStatus rbList
  logInfo $ "rbList: test " <> show rbList
  BookingListRes <$> traverse (`SRB.buildBookingAPIEntity` personId) rbList

addStop :: (Id Person.Person, Id Merchant) -> Id SRB.Booking -> StopReq -> Flow APISuccess
addStop (_, merchantId) bookingId req = do
  processStop bookingId req merchantId False
  pure Success

editStop :: (Id Person.Person, Id Merchant) -> Id SRB.Booking -> StopReq -> Flow APISuccess
editStop (_, merchantId) bookingId req = do
  processStop bookingId req merchantId True
  pure Success

processStop :: Id SRB.Booking -> StopReq -> Id Merchant -> Bool -> Flow ()
processStop bookingId loc merchantId isEdit = do
  booking <- runInReplica $ QRB.findById bookingId >>= fromMaybeM (BookingNotFound bookingId.getId)
  uuid <- generateGUID
  validateStopReq booking isEdit
  location <- buildLocation loc
  prevOrder <- QLM.maxOrderByEntity booking.id.getId
  locationMapping <- buildLocationMapping location.id booking.id.getId isEdit (Just booking.merchantId) (Just booking.merchantOperatingCityId) prevOrder
  QL.create location
  QLM.create locationMapping
  QRB.updateStop booking (Just location)
  bppBookingId <- booking.bppBookingId & fromMaybeM (BookingFieldNotPresent "bppBookingId")
  merchant <- CQMerchant.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  let details =
        if isEdit
          then do
            let stopLocation = location{id = Id $ show prevOrder}
            ACL.UEditStopBuildReqDetails $
              ACL.EditStopBuildReqDetails
                { stops = [stopLocation]
                }
          else do
            let stopLocation = location{id = Id $ show (prevOrder + 1)}
            ACL.UAddStopBuildReqDetails $
              ACL.AddStopBuildReqDetails
                { stops = [stopLocation]
                }
  let dUpdateReq =
        ACL.UpdateBuildReq
          { bppId = booking.providerId,
            bppUrl = booking.providerUrl,
            transactionId = booking.transactionId,
            messageId = uuid,
            city = merchant.defaultCity, -- TODO: Correct during interoperability
            ..
          }
  becknUpdateReq <- ACL.buildUpdateReq dUpdateReq
  void . withShortRetry $ CallBPP.updateV2 booking.providerUrl becknUpdateReq

validateStopReq :: (MonadFlow m) => SRB.Booking -> Bool -> m ()
validateStopReq booking isEdit = do
  unless (booking.status `elem` SRB.activeBookingStatus) $ throwError (RideInvalidStatus $ "Cannot edit/add stop in this booking " <> booking.id.getId)
  case booking.bookingDetails of
    SRB.OneWayDetails _ -> throwError $ RideInvalidStatus "Cannot add/edit stop in static offer on demand rides"
    SRB.RentalDetails SRB.RentalBookingDetails {..} ->
      if isEdit
        then unless (isJust stopLocation) $ throwError (InvalidRequest $ "Can't find stop to be edited " <> booking.id.getId)
        else unless (isNothing stopLocation) $ throwError (InvalidRequest $ "Can't add next stop before reaching previous stop " <> booking.id.getId)
    SRB.DriverOfferDetails _ -> throwError $ RideInvalidStatus "Cannot add/edit stop in dynamic offer on demand rides"
    SRB.OneWaySpecialZoneDetails _ -> throwError $ RideInvalidStatus "Cannot add/edit stop in special zone rides"
    SRB.InterCityDetails _ -> throwError $ RideInvalidStatus "Cannot add/edit stop in intercity rides"

buildLocation :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => StopReq -> m Location
buildLocation req = do
  id <- generateGUID
  now <- getCurrentTime
  return $
    Location
      { lat = req.gps.lat,
        lon = req.gps.lon,
        address = req.address,
        createdAt = now,
        updatedAt = now,
        ..
      }

buildLocationMapping :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Location -> Text -> Bool -> Maybe (Id DM.Merchant) -> Maybe (Id DMOC.MerchantOperatingCity) -> Int -> m DLM.LocationMapping
buildLocationMapping locationId entityId isEdit merchantId merchantOperatingCityId prevOrder = do
  id <- generateGUID
  now <- getCurrentTime
  when isEdit $ QLM.updatePastMappingVersions entityId prevOrder
  let version = QLM.latestTag
      tag = DLM.BOOKING
  return $
    DLM.LocationMapping
      { order = if isEdit then prevOrder else prevOrder + 1,
        createdAt = now,
        updatedAt = now,
        ..
      }
