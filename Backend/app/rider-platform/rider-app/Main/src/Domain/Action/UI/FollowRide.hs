{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Action.UI.FollowRide where

import API.Types.UI.FollowRide
import Data.OpenApi (ToSchema)
import Domain.Action.UI.Profile (getDefaultEmergencyNumbers, sendNotificationToEmergencyContact)
import Domain.Types.Booking
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.Person as Person
import qualified Domain.Types.Person.PersonDefaultEmergencyNumber as PDEN
import Environment
import qualified Environment
import EulerHS.Prelude hiding (elem, id)
import Kernel.Beam.Functions
import Kernel.External.Encryption
import qualified Kernel.External.Notification as Notification
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess as APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import Servant
import qualified Storage.Beam.Person as BeamP
import qualified Storage.CachedQueries.FollowRide as CQFollowRide
import qualified Storage.Queries.Booking as Booking
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Person.PersonDefaultEmergencyNumber as PDEN
import Tools.Auth
import Tools.Error

getFollowRide :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r, EncFlow m r, Log m) => (Maybe (Id Person.Person), Id Merchant.Merchant) -> m [Followers]
getFollowRide (mbPersonId, _) = do
  id <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  emContacts <- PDEN.findAllByContactPersonId id
  let follwingEmContacts = filter (.enableForFollowing) emContacts
  foldlM
    ( \acc contact -> do
        mbPerson <- QPerson.findById contact.personId
        case mbPerson of
          Nothing -> pure acc
          Just person -> do
            phoneNo <- mapM decrypt person.mobileNumber
            mbBookingId <- Booking.findActiveBookingIdByRiderId person.id
            case mbBookingId of
              Nothing -> pure acc
              Just bookingId -> do
                let follower = buildFollower phoneNo person.firstName bookingId contact.priority
                pure $ acc <> [follower]
    )
    []
    follwingEmContacts

buildFollower :: Maybe Text -> Maybe Text -> Id Booking -> Int -> Followers
buildFollower phoneNo name bookingId priority =
  Followers
    { bookingId,
      mobileNumber = fromMaybe "" phoneNo,
      name,
      priority
    }

postShareRide :: (Maybe (Id Person.Person), Id Merchant.Merchant) -> ShareRideReq -> Flow APISuccess.APISuccess
postShareRide (mbPersonId, merchantId) req = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  person <- QP.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  emergencyContacts <- getDefaultEmergencyNumbers (personId, merchantId)
  void $
    mapM
      ( \emergencyContact ->
          when (emergencyContact.mobileNumber `elem` req.emergencyContactNumbers) $
            sendNotificationToEmergencyContact person title (body person) Notification.SHARE_RIDE
      )
      emergencyContacts.defaultEmergencyNumbers
  return APISuccess.Success
  where
    title = "Ride Share"
    body person = personName person <> " has invited you to follow their ride! Follow along to ensure their safety."
    personName person = (fromMaybe "" person.firstName) <> " " <> (fromMaybe "" person.lastName)
