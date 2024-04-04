{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Action.UI.FollowRide where

import API.Types.UI.FollowRide
import Data.OpenApi (ToSchema)
import Data.Text (unwords)
import Data.Time hiding (secondsToNominalDiffTime)
import qualified Domain.Action.UI.PersonDefaultEmergencyNumber as PDEN
import Domain.Action.UI.Profile as DAP
import Domain.Types.Booking
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.Person as Person
import qualified Domain.Types.PersonDefaultEmergencyNumber as PDEN
import qualified Domain.Types.RiderConfig as DRC
import Environment
import qualified Environment
import EulerHS.Prelude hiding (elem, id, unwords)
import Kernel.Beam.Functions
import Kernel.External.Encryption
import qualified Kernel.External.Notification as Notification
import Kernel.Prelude hiding (mapM_, unwords)
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess as APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import Servant
import SharedLogic.Person as SLP
import SharedLogic.PersonDefaultEmergencyNumber as SLPEN
import qualified Storage.Beam.Person as BeamP
import qualified Storage.CachedQueries.FollowRide as CQFollowRide
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.RiderConfig as QRC
import qualified Storage.Queries.Booking as Booking
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.PersonDefaultEmergencyNumber as PDEN
import Tools.Auth
import Tools.Error
import Tools.Notifications

getFollowRide :: (KvDbFlow m r, EncFlow m r, Log m) => (Maybe (Id Person.Person), Id Merchant.Merchant) -> m [Followers]
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
                let follower = buildFollower phoneNo person.firstName bookingId contact.priority
                pure $ acc <> [follower]
    )
    []
    followingEmContacts

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
              SLPEN.sendNotificationToEmergencyContact emergencyContactEntity (body person) title Notification.SHARE_RIDE
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
