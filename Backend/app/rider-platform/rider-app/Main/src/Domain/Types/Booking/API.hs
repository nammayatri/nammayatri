{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.Booking.API where

import Data.OpenApi (ToSchema (..), genericDeclareNamedSchema)
import Domain.Types.Booking.Type
import qualified Domain.Types.Exophone as DExophone
import Domain.Types.FarePolicy.FareBreakup
import qualified Domain.Types.FarePolicy.FareBreakup as DFareBreakup
import Domain.Types.Location (LocationAPIEntity)
import qualified Domain.Types.Location as SLoc
import qualified Domain.Types.Merchant.MerchantPaymentMethod as DMPM
import qualified Domain.Types.RentalDetails as DRentalDetails
import Domain.Types.Ride (Ride, RideAPIEntity, makeRideAPIEntity)
import qualified Domain.Types.Ride as DRide
import EulerHS.Prelude hiding (id, null)
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.Exophone as CQExophone
import qualified Storage.CachedQueries.Merchant.MerchantPaymentMethod as CQMPM
import qualified Storage.Queries.FareBreakup as QFareBreakup
import qualified Storage.Queries.Ride as QRide
import Tools.Error
import qualified Tools.JSON as J
import qualified Tools.Schema as S

data BookingAPIEntity = BookingAPIEntity
  { id :: Id Booking,
    status :: BookingStatus,
    agencyName :: Text,
    agencyNumber :: Text,
    estimatedFare :: Money,
    discount :: Maybe Money,
    estimatedTotalFare :: Money,
    fromLocation :: LocationAPIEntity,
    rideList :: [RideAPIEntity],
    tripTerms :: [Text],
    fareBreakup :: [FareBreakupAPIEntity],
    bookingDetails :: BookingAPIDetails,
    rideStartTime :: Maybe UTCTime,
    rideEndTime :: Maybe UTCTime,
    duration :: Maybe Seconds,
    merchantExoPhone :: Text,
    specialLocationTag :: Maybe Text,
    paymentMethod :: Maybe DMPM.PaymentMethodAPIEntity,
    paymentUrl :: Maybe Text,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

-- do not change constructor names without changing fareProductConstructorModifier
data BookingAPIDetails
  = OneWayAPIDetails OneWayBookingAPIDetails
  | RentalAPIDetails DRentalDetails.RentalDetailsAPIEntity
  | DriverOfferAPIDetails OneWayBookingAPIDetails
  | OneWaySpecialZoneAPIDetails OneWaySpecialZoneBookingAPIDetails
  deriving (Show, Generic)

instance ToJSON BookingAPIDetails where
  toJSON = genericToJSON J.fareProductOptions

instance FromJSON BookingAPIDetails where
  parseJSON = genericParseJSON J.fareProductOptions

instance ToSchema BookingAPIDetails where
  declareNamedSchema = genericDeclareNamedSchema S.fareProductSchemaOptions

data OneWayBookingAPIDetails = OneWayBookingAPIDetails
  { toLocation :: LocationAPIEntity,
    estimatedDistance :: HighPrecMeters
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

data OneWaySpecialZoneBookingAPIDetails = OneWaySpecialZoneBookingAPIDetails
  { toLocation :: LocationAPIEntity,
    estimatedDistance :: HighPrecMeters,
    otpCode :: Maybe Text
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

makeBookingAPIEntity ::
  Booking ->
  Maybe Ride ->
  [Ride] ->
  [FareBreakup] ->
  Maybe DExophone.Exophone ->
  Maybe DMPM.MerchantPaymentMethod ->
  BookingAPIEntity
makeBookingAPIEntity booking activeRide allRides fareBreakups mbExophone mbPaymentMethod = do
  let bookingDetails = mkBookingAPIDetails booking.bookingDetails
  BookingAPIEntity
    { id = booking.id,
      status = booking.status,
      agencyName = booking.providerName,
      agencyNumber = booking.providerMobileNumber,
      estimatedFare = booking.estimatedFare,
      discount = booking.discount,
      estimatedTotalFare = booking.estimatedTotalFare,
      fromLocation = SLoc.makeLocationAPIEntity booking.fromLocation,
      rideList = allRides <&> makeRideAPIEntity,
      tripTerms = fromMaybe [] $ booking.tripTerms <&> (.descriptions),
      fareBreakup = DFareBreakup.mkFareBreakupAPIEntity <$> fareBreakups,
      bookingDetails,
      rideStartTime = activeRide >>= (.rideStartTime),
      rideEndTime = activeRide >>= (.rideEndTime),
      duration = getRideDuration activeRide,
      merchantExoPhone = maybe booking.primaryExophone (\exophone -> if not exophone.isPrimaryDown then exophone.primaryPhone else exophone.backupPhone) mbExophone,
      specialLocationTag = booking.specialLocationTag,
      paymentMethod = DMPM.mkPaymentMethodAPIEntity <$> mbPaymentMethod,
      paymentUrl = booking.paymentUrl,
      createdAt = booking.createdAt,
      updatedAt = booking.updatedAt
    }
  where
    getRideDuration :: Maybe DRide.Ride -> Maybe Seconds
    getRideDuration mbRide = do
      ride <- mbRide
      startTime <- ride.rideStartTime
      endTime <- ride.rideEndTime
      return $ nominalDiffTimeToSeconds $ diffUTCTime endTime startTime

    mkBookingAPIDetails :: BookingDetails -> BookingAPIDetails
    mkBookingAPIDetails = \case
      OneWayDetails details -> OneWayAPIDetails . mkOneWayAPIDetails $ details
      RentalDetails DRentalDetails.RentalDetails {..} -> RentalAPIDetails DRentalDetails.RentalDetailsAPIEntity {bppQuoteId = id.getId, ..}
      DriverOfferDetails details -> DriverOfferAPIDetails . mkOneWayAPIDetails $ details
      OneWaySpecialZoneDetails details -> OneWaySpecialZoneAPIDetails . mkOneWaySpecialZoneAPIDetails $ details
      where
        mkOneWayAPIDetails OneWayBookingDetails {..} =
          OneWayBookingAPIDetails
            { toLocation = SLoc.makeLocationAPIEntity toLocation,
              estimatedDistance = distance
            }
        mkOneWaySpecialZoneAPIDetails OneWaySpecialZoneBookingDetails {..} =
          OneWaySpecialZoneBookingAPIDetails
            { toLocation = SLoc.makeLocationAPIEntity toLocation,
              estimatedDistance = distance,
              ..
            }

buildBookingAPIEntity :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) => Booking -> m BookingAPIEntity
buildBookingAPIEntity booking = do
  mbActiveRide <- runInReplica $ QRide.findActiveByRBId booking.id
  mbRide <- runInReplica $ QRide.findByRBId booking.id
  fareBreakups <- runInReplica $ QFareBreakup.findAllByBookingId booking.id
  mbExoPhone <- CQExophone.findByPrimaryPhone booking.primaryExophone
  mbPaymentMethod <- forM booking.paymentMethodId $ \paymentMethodId -> do
    CQMPM.findByIdAndMerchantId paymentMethodId booking.merchantId
      >>= fromMaybeM (MerchantPaymentMethodNotFound paymentMethodId.getId)
  return $ makeBookingAPIEntity booking mbActiveRide (maybeToList mbRide) fareBreakups mbExoPhone mbPaymentMethod
