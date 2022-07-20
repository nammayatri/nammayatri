module Domain.Types.Booking.API where

import Beckn.External.Encryption
import Beckn.Types.Amount
import Beckn.Types.Common hiding (id)
import Beckn.Types.Id
import Beckn.Utils.Common
import Data.Aeson
import Data.OpenApi
import Domain.Types.Booking.Type
import Domain.Types.BookingLocation
import qualified Domain.Types.BookingLocation as DLoc
import Domain.Types.FarePolicy.FareBreakup
import qualified Domain.Types.FarePolicy.FareBreakup as DFareBreakup
import qualified Domain.Types.FarePolicy.RentalFarePolicy as DRentalFP
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.Vehicle as DVeh
import EulerHS.Prelude hiding (id)
import qualified Storage.Queries.BookingLocation as QBLoc
import qualified Storage.Queries.FarePolicy.FareBreakup as QFareBreakup
import qualified Storage.Queries.FarePolicy.RentalFarePolicy as QRentalFP
import qualified Storage.Queries.Ride as QRide
import qualified Tools.JSON as J
import qualified Tools.Schema as S
import Types.Error

data RideBookingAPIEntity = RideBookingAPIEntity
  { id :: Id RideBooking,
    status :: RideBookingStatus,
    estimatedFare :: Amount,
    discount :: Maybe Amount,
    estimatedTotalFare :: Amount,
    fromLocation :: DLoc.BookingLocationAPIEntity,
    rideList :: [DRide.RideAPIEntity],
    tripTerms :: [Text],
    fareBreakup :: [FareBreakupAPIEntity],
    bookingDetails :: RideBookingDetailsAPIEntity,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

-- do not change constructor names without changing fareProductConstructorModifier
data RideBookingDetailsAPIEntity = OneWayDetailsAPIEntity OneWayBookingDetailsAPIEntity | RentalDetailsAPIEntity RentalBookingDetailsAPIEntity
  deriving (Show, Generic)

instance ToJSON RideBookingDetailsAPIEntity where
  toJSON = genericToJSON J.fareProductOptions

instance FromJSON RideBookingDetailsAPIEntity where
  parseJSON = genericParseJSON J.fareProductOptions

instance ToSchema RideBookingDetailsAPIEntity where
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

buildRideBookingAPIEntity :: (EsqDBFlow m r, EncFlow m r) => RideBooking -> m RideBookingAPIEntity
buildRideBookingAPIEntity booking = do
  fromLocation <- QBLoc.findById booking.fromLocationId >>= fromMaybeM LocationNotFound
  let rbStatus = booking.status
  now <- getCurrentTime
  rideAPIEntityList <- mapM (buildRideAPIEntity now) =<< QRide.findAllRideAPIEntityDataByRBId booking.id
  fareBreakups <- QFareBreakup.findAllByRideBookingId booking.id
  (bookingDetails, tripTerms) <- buildRideBookingAPIDetails booking.rideBookingDetails

  return $
    RideBookingAPIEntity
      { id = booking.id,
        status = rbStatus,
        estimatedFare = booking.estimatedFare,
        discount = booking.discount,
        estimatedTotalFare = booking.estimatedTotalFare,
        fromLocation = DLoc.makeBookingLocationAPIEntity fromLocation,
        rideList = rideAPIEntityList,
        fareBreakup = DFareBreakup.mkFareBreakupAPIEntity <$> fareBreakups,
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

    buildRideBookingAPIDetails :: EsqDBFlow m r => RideBookingDetails -> m (RideBookingDetailsAPIEntity, [Text])
    buildRideBookingAPIDetails = \case
      OneWayDetails OneWayRideBookingDetails {..} -> do
        toLocation' <- QBLoc.findById toLocationId >>= fromMaybeM LocationNotFound
        let details =
              OneWayDetailsAPIEntity
                OneWayBookingDetailsAPIEntity
                  { toLocation = makeBookingLocationAPIEntity toLocation'
                  }
        pure (details, [])
      RentalDetails (RentalRideBookingDetails rentalFarePolicyId) -> do
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
          organizationId = Nothing,
          deviceToken = Nothing,
          description = Nothing,
          createdAt = now,
          updatedAt = now
        }
    vehicleDefault now =
      DVeh.Vehicle
        { driverId = Id "[Vehicle deleted]",
          organizationId = Id "N/A",
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
          createdAt = now,
          updatedAt = now
        }
