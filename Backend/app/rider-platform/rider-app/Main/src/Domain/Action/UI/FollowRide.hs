module Domain.Action.UI.FollowRide where

import API.Types.UI.FollowRide
import qualified Domain.Action.UI.Booking as DAB
import qualified Domain.Action.UI.PersonDefaultEmergencyNumber as PDEN
import Domain.Action.UI.Profile as DAP
import Domain.Types.Booking
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.Person as Person
import qualified Domain.Types.Ride as DRide
import Environment
import EulerHS.Prelude hiding (elem, id, unpack, unwords)
import Kernel.Beam.Functions
import Kernel.External.Encryption
import qualified Kernel.External.Notification as Notification
import Kernel.Prelude hiding (mapM_, unwords)
import qualified Kernel.Storage.Hedis as Hedis
import qualified Kernel.Types.APISuccess as APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.Person as SLP
import SharedLogic.PersonDefaultEmergencyNumber as SLPEN
import qualified Storage.CachedQueries.FollowRide as CQFollowRide
import qualified Storage.Queries.Booking as Booking
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.PersonDefaultEmergencyNumber as PDEN
import qualified Storage.Queries.Ride as QRide
import Tools.Error

getFollowRide :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r, EncFlow m r, Log m) => (Maybe (Id Person.Person), Id Merchant.Merchant) -> m [Followers]
getFollowRide (mbPersonId, _) = do
  id <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  emContacts <- PDEN.findAllByContactPersonId id
  followList <- CQFollowRide.getFollowRideCounter id
  let followingEmContacts = filter (\contact -> contact.personId.getId `elem` followList) emContacts
  foldlM
    ( \acc contact -> do
        mbPerson <- QPerson.findById contact.personId
        case mbPerson of
          Nothing -> pure acc
          Just person -> do
            phoneNo <- mapM decrypt person.mobileNumber
            mbBookingId <- Booking.findByRiderId person.id
            case mbBookingId of
              Nothing -> pure acc
              Just bookingId -> do
                let follower = buildFollower phoneNo person bookingId contact.priority
                pure $ acc <> [follower]
    )
    []
    followingEmContacts

buildFollower :: Maybe Text -> Person.Person -> Id Booking -> Int -> Followers
buildFollower phoneNo person bookingId priority =
  Followers
    { bookingId,
      mobileNumber = fromMaybe "" phoneNo,
      name = person.firstName,
      priority,
      personId = person.id
    }

postShareRide :: (Maybe (Id Person.Person), Id Merchant.Merchant) -> ShareRideReq -> Flow APISuccess.APISuccess
postShareRide (mbPersonId, merchantId) req = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  person <- QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  emergencyContacts <- DAP.getDefaultEmergencyNumbers (personId, merchantId)
  mapM_
    ( \emergencyContact ->
        when (emergencyContact.mobileNumber `elem` req.emergencyContactNumbers) $ do
          case emergencyContact.contactPersonId of
            Nothing -> pure ()
            Just id -> do
              emergencyContactEntity <- QPerson.findById id >>= fromMaybeM (PersonDoesNotExist id.getId)
              updateFollowDetails emergencyContactEntity emergencyContact
              dbHash <- getDbHash emergencyContact.mobileNumber
              PDEN.updateShareRide dbHash personId True
              SLPEN.sendNotificationToEmergencyContact personId emergencyContactEntity (body person) title Notification.SHARE_RIDE Nothing
    )
    emergencyContacts.defaultEmergencyNumbers
  return APISuccess.Success
  where
    title = "Ride Share"
    body person = SLP.getName person <> " has invited you to follow their ride!\nFollow along to ensure their safety."

updateFollowDetails :: Person.Person -> PDEN.PersonDefaultEmergencyNumberAPIEntity -> Flow ()
updateFollowDetails contactPersonEntity PDEN.PersonDefaultEmergencyNumberAPIEntity {..} = do
  void $ CQFollowRide.updateFollowRideList contactPersonEntity.id personId True
  QPerson.updateFollowsRide True contactPersonEntity.id

getFollowRideECStatus :: (Maybe (Id Person.Person), Id Merchant.Merchant) -> Id DRide.Ride -> Flow EmergencyContactsStatusRes
getFollowRideECStatus (mbPersonId, _merchantId) rideId = do
  _personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  ride <- runInReplica $ QRide.findById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
  let hashKey = DAB.makeEmergencyContactSOSCacheKey ride.id
  keyValues <- Kernel.Prelude.map (\(personIdText, utcTime) -> ContactsDetail (Id personIdText) utcTime) <$> Hedis.hGetAll hashKey
  return $ EmergencyContactsStatusRes keyValues

getFollowRideCustomerDetails :: (Maybe (Id Person.Person), Id Merchant.Merchant) -> Id DRide.Ride -> Flow FollowRideCustomerDetailsRes
getFollowRideCustomerDetails (_, _) rideId = do
  ride <- QRide.findById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
  booking <- Booking.findByPrimaryKey ride.bookingId >>= fromMaybeM (BookingDoesNotExist ride.bookingId.getId)
  person <- QPerson.findById booking.riderId >>= fromMaybeM (PersonDoesNotExist booking.riderId.getId)
  customerPhone <- mapM decrypt person.mobileNumber
  return $ FollowRideCustomerDetailsRes {bookingId = ride.bookingId, customerName = SLP.getName person, ..}
