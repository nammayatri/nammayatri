{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Beckn.OnInit where

import Domain.Types.Booking (BPPBooking, Booking)
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.FarePolicy.FareBreakup as DFareBreakup
import qualified Domain.Types.Location as DL
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Ticket as DTT
import qualified Domain.Types.VehicleVariant as Veh
import Kernel.External.Encryption (decrypt)
import Kernel.Prelude
import Kernel.Storage.Hedis (HedisFlow)
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.Booking as QRideB
import qualified Storage.Queries.FareBreakup as QFareBreakup
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Ticket as QRideT
import Tools.Error

data OnInitReq = OnInitReq
  { bookingId :: Maybe (Id Booking),
    bppBookingId :: Maybe (Id BPPBooking),
    ticketId :: Maybe (Id DTT.Ticket),
    estimatedFare :: Money,
    discount :: Maybe Money,
    fareBreakup :: Maybe [OnInitFareBreakup],
    estimatedTotalFare :: Money,
    paymentUrl :: Maybe Text
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

data OnInitRes = OnInitRes
  { bookingId :: Maybe (Id DRB.Booking),
    bppBookingId :: Maybe (Id DRB.BPPBooking),
    bookingDetails :: Maybe DRB.BookingDetails,
    ticketId :: Maybe (Id DTT.Ticket),
    driverId :: Maybe Text,
    paymentUrl :: Maybe Text,
    vehicleVariant :: Veh.VehicleVariant,
    itemId :: Text,
    fulfillmentId :: Maybe Text,
    bppId :: Text,
    bppUrl :: BaseUrl,
    fromLocation :: DL.Location,
    mbToLocation :: Maybe DL.Location,
    estimatedTotalFare :: Money,
    estimatedFare :: Money,
    riderPhoneCountryCode :: Maybe Text, -- Nohthing in FRFS
    riderPhoneNumber :: Maybe Text, -- Nohthing in FRFS
    mbRiderName :: Maybe Text,
    transactionId :: Text,
    merchant :: DM.Merchant,
    city :: Context.City
  }
  deriving (Generic, Show)

data FareBreakup = FareBreakup
  { totalFare :: Text,
    basicFare :: Text,
    discount :: Text,
    cgst :: Text,
    sgst :: Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON, ToSchema)

data OnInitFareBreakup = OnInitFareBreakup
  { amount :: HighPrecMoney,
    description :: Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON, ToSchema)

onInit :: (CacheFlow m r, EsqDBFlow m r, EncFlow m r, HedisFlow m r) => OnInitReq -> m OnInitRes
onInit req = do
  case req.bookingId of
    Just bookingId -> do
      -- booking <- QRideB.findById bookingId >>= fromMaybeM (BookingDoesNotExist bookingId.getId)
      onInitRide req bookingId
    Nothing ->
      case req.ticketId of
        Just ticketId -> do
          ticket <- QRideT.findById ticketId >>= fromMaybeM (TicketDoesNotExist ticketId.getId)
          onInitBus req ticket
        Nothing -> throwError $ InvalidRequest "Both bookingId and ticketId are missing"

onInitRide :: (CacheFlow m r, EsqDBFlow m r, EncFlow m r, HedisFlow m r) => OnInitReq -> Id Booking -> m OnInitRes
onInitRide req bookingId = do
  void $ QRideB.updateBPPBookingId bookingId (fromJust req.bppBookingId)
  void $ QRideB.updatePaymentInfo bookingId req.estimatedFare req.discount req.estimatedTotalFare req.paymentUrl
  booking <- QRideB.findById bookingId >>= fromMaybeM (BookingDoesNotExist bookingId.getId)
  merchant <- CQM.findById booking.merchantId >>= fromMaybeM (MerchantNotFound booking.merchantId.getId)
  decRider <- QP.findById booking.riderId >>= fromMaybeM (PersonNotFound booking.riderId.getId) >>= decrypt
  riderPhoneCountryCode <- Just (decRider.mobileCountryCode) & fromMaybeM (PersonFieldNotPresent "mobileCountryCode")
  riderPhoneNumber <- Just (decRider.mobileNumber) & fromMaybeM (PersonFieldNotPresent "mobileNumber")
  bppBookingId <- Just (req.bppBookingId) & fromMaybeM (BookingFieldNotPresent "bppBookingId")
  city <-
    CQMOC.findById booking.merchantOperatingCityId
      >>= fmap (.city) . fromMaybeM (MerchantOperatingCityNotFound booking.merchantOperatingCityId.getId)
  let fromLocation = booking.fromLocation
  let mbToLocation = case booking.bookingDetails of
        DRB.RentalDetails _ -> Nothing
        DRB.OneWayDetails details -> Just details.toLocation
        DRB.DriverOfferDetails details -> Just details.toLocation
        DRB.OneWaySpecialZoneDetails details -> Just details.toLocation
  return $
    OnInitRes
      { bookingId = Just booking.id,
        driverId = booking.driverId,
        paymentUrl = booking.paymentUrl,
        itemId = booking.itemId,
        vehicleVariant = booking.vehicleVariant,
        fulfillmentId = booking.fulfillmentId,
        bookingDetails = Just booking.bookingDetails,
        bppId = booking.providerId,
        bppUrl = booking.providerUrl,
        estimatedTotalFare = booking.estimatedTotalFare,
        estimatedFare = booking.estimatedFare,
        fromLocation = fromLocation,
        mbToLocation = mbToLocation,
        mbRiderName = decRider.firstName,
        transactionId = booking.transactionId,
        merchant = merchant,
        ticketId = Nothing,
        ..
      }

onInitBus :: (CacheFlow m r, EsqDBFlow m r, EncFlow m r, HedisFlow m r) => OnInitReq -> DTT.Ticket -> m OnInitRes
onInitBus req ticket = do
  moc <- CQMOC.findById ticket.merchantOperatingCityId >>= fromMaybeM (MerchantOperatingCityDoesNotExist ticket.merchantOperatingCityId.getId)
  merchant <- CQM.findById moc.merchantId >>= fromMaybeM (MerchantNotFound moc.merchantId.getId)
  let fromLocation = ticket.fromLocation
      riderPhoneCountryCode = Nothing
      riderPhoneNumber = Nothing
      bppBookingId = Nothing
  fareBreakups <- req.fareBreakup & fromMaybeM (InvalidRequest "Fare Breakup not found")
  breakups <- traverse (buildFareBreakup ticket.id) fareBreakups
  _ <- QFareBreakup.createMany breakups
  void $ QRideT.updateStatus ticket.id DTT.APPROVED
  return $
    OnInitRes
      { ticketId = Just ticket.id,
        estimatedFare = ticket.pricePerAdult,
        itemId = ticket.itemId,
        estimatedTotalFare = ticket.pricePerAdult * fromIntegral ticket.quantity,
        fulfillmentId = ticket.fulfillmentId,
        fromLocation = fromLocation,
        mbToLocation = Nothing,
        bppId = ticket.providerId,
        bppUrl = ticket.providerUrl,
        vehicleVariant = Veh.BUS,
        transactionId = ticket.searchRequestId,
        bookingId = Nothing,
        bookingDetails = Nothing,
        driverId = Nothing,
        paymentUrl = Nothing,
        mbRiderName = Nothing,
        city = moc.city,
        ..
      }
  where
    buildFareBreakup :: MonadFlow m => Id DTT.Ticket -> OnInitFareBreakup -> m DFareBreakup.FareBreakup
    buildFareBreakup ticketId OnInitFareBreakup {..} = do
      guid <- generateGUID
      let bookingId = "" -- TODO : Remove this field in the future
      pure
        DFareBreakup.FareBreakup
          { id = guid,
            entityType = DFareBreakup.BOOKING,
            entityId = Just ticketId,
            ..
          }
