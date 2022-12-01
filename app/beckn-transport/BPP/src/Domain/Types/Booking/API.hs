module Domain.Types.Booking.API where

import Beckn.External.Encryption
import Beckn.Types.Common hiding (id)
import Beckn.Types.Id
import Beckn.Utils.Common
import Data.Aeson
import Data.OpenApi
import Domain.Types.Booking.BookingLocation
import qualified Domain.Types.Booking.BookingLocation as DLoc
import Domain.Types.Booking.Type
import Domain.Types.FarePolicy.FareBreakup
import qualified Domain.Types.FarePolicy.FareBreakup as DFareBreakup
import qualified Domain.Types.FarePolicy.RentalFarePolicy as DRentalFP
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.Vehicle as DVeh
import EulerHS.Prelude hiding (id)
import Storage.CachedQueries.CacheConfig
import qualified Storage.CachedQueries.FarePolicy.RentalFarePolicy as QRentalFP
import qualified Storage.Queries.FarePolicy.FareBreakup as QFareBreakup
import qualified Storage.Queries.Ride as QRide
import Tools.Error
import qualified Tools.JSON as J
import qualified Tools.Schema as S

data BookingAPIEntity = BookingAPIEntity
  { id :: Id Booking,
    status :: BookingStatus,
    estimatedFare :: Money,
    discount :: Maybe Money,
    estimatedTotalFare :: Money,
    fromLocation :: DLoc.BookingLocationAPIEntity,
    rideList :: [DRide.RideAPIEntity],
    tripTerms :: [Text],
    fareBreakup :: [FareBreakupAPIEntity],
    riderName :: Maybe Text,
    bookingDetails :: BookingDetailsAPIEntity,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

-- do not change constructor names without changing fareProductConstructorModifier
data BookingDetailsAPIEntity = OneWayDetailsAPIEntity OneWayBookingDetailsAPIEntity | RentalDetailsAPIEntity RentalBookingDetailsAPIEntity
  deriving (Show, Generic)

instance ToJSON BookingDetailsAPIEntity where
  toJSON = genericToJSON J.fareProductOptions

instance FromJSON BookingDetailsAPIEntity where
  parseJSON = genericParseJSON J.fareProductOptions

instance ToSchema BookingDetailsAPIEntity where
  declareNamedSchema = genericDeclareNamedSchema S.fareProductSchemaOptions

newtype OneWayBookingDetailsAPIEntity = OneWayBookingDetailsAPIEntity
  { toLocation :: BookingLocationAPIEntity
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

data RentalBookingDetailsAPIEntity = RentalBookingDetailsAPIEntity
  { baseDistance :: Kilometers,
    baseDuration :: Hours
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

buildBookingAPIEntity :: (CacheFlow m r, EsqDBFlow m r, EncFlow m r) => Booking -> m BookingAPIEntity
buildBookingAPIEntity booking = do
  let rbStatus = booking.status
  now <- getCurrentTime
  rideAPIEntityList <- mapM (buildRideAPIEntity now) =<< QRide.findAllRideAPIEntityDataByRBId booking.id
  fareBreakups <- QFareBreakup.findAllByBookingId booking.id
  (bookingDetails, tripTerms) <- buildBookingAPIDetails booking.bookingDetails

  return $
    BookingAPIEntity
      { id = booking.id,
        status = rbStatus,
        estimatedFare = booking.estimatedFare,
        discount = booking.discount,
        estimatedTotalFare = booking.estimatedTotalFare,
        fromLocation = DLoc.makeBookingLocationAPIEntity booking.fromLocation,
        rideList = rideAPIEntityList,
        fareBreakup = DFareBreakup.mkFareBreakupAPIEntity <$> fareBreakups,
        riderName = booking.riderName,
        bookingDetails,
        tripTerms,
        createdAt = booking.createdAt,
        updatedAt = booking.updatedAt
      }
  where
    buildRideAPIEntity :: (EsqDBFlow m r, EncFlow m r) => UTCTime -> (DRide.Ride, Maybe DVeh.Vehicle, Maybe DP.Person) -> m DRide.RideAPIEntity
    buildRideAPIEntity now (ride, mbVehicle, mbDriver) = do
      let vehicle = fromMaybe (vehicleDefault now) mbVehicle
      decDriver <- maybe (return $ driverDefault now) decrypt mbDriver
      return $ DRide.makeRideAPIEntity ride decDriver vehicle

    buildBookingAPIDetails :: (CacheFlow m r, EsqDBFlow m r) => BookingDetails -> m (BookingDetailsAPIEntity, [Text])
    buildBookingAPIDetails = \case
      OneWayDetails OneWayBookingDetails {..} -> do
        let details =
              OneWayDetailsAPIEntity
                OneWayBookingDetailsAPIEntity
                  { toLocation = makeBookingLocationAPIEntity toLocation
                  }
        pure (details, [])
      RentalDetails (RentalBookingDetails rentalFarePolicyId) -> do
        DRentalFP.RentalFarePolicy {..} <-
          QRentalFP.findById rentalFarePolicyId
            >>= fromMaybeM NoRentalFarePolicy
        let details = RentalDetailsAPIEntity RentalBookingDetailsAPIEntity {..}
        pure (details, descriptions)

    driverDefault now =
      DP.Person
        { id = Id "[Driver deleted]",
          firstName = "[Driver deleted]",
          middleName = Nothing,
          lastName = Nothing,
          role = DP.DRIVER,
          gender = DP.FEMALE,
          identifierType = DP.EMAIL,
          email = Nothing,
          mobileNumber = Just "N/A",
          mobileCountryCode = Nothing,
          passwordHash = Nothing,
          identifier = Nothing,
          rating = Nothing,
          isNew = False,
          merchantId = Nothing,
          deviceToken = Nothing,
          description = Nothing,
          createdAt = now,
          updatedAt = now
        }
    vehicleDefault now =
      DVeh.Vehicle
        { driverId = Id "[Vehicle deleted]",
          merchantId = Id "N/A",
          variant = DVeh.SEDAN,
          model = "N/A",
          color = "N/A",
          registrationNo = "N/A",
          capacity = Nothing,
          category = Nothing,
          make = Nothing,
          size = Nothing,
          energyType = Nothing,
          registrationCategory = Nothing,
          vehicleClass = "3WT",
          createdAt = now,
          updatedAt = now
        }
