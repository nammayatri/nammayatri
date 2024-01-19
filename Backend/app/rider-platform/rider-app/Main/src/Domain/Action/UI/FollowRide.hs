{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Action.UI.FollowRide where

import API.Types.UI.FollowRide
import qualified API.Types.UI.FollowRide
import Data.OpenApi (ToSchema)
import Domain.Types.Booking
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Domain.Types.Person.PersonDefaultEmergencyNumber as PDEN
import qualified Environment
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Id
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import Servant
import qualified Storage.Beam.Person as BeamP
import qualified Storage.CachedQueries.FollowRide as CQFollowRide
import qualified Storage.Queries.Booking as Booking
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Person.PersonDefaultEmergencyNumber as PDEN
import Tools.Auth

getFollowRide :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r, EncFlow m r, Log m) => (Kernel.Types.Id.Id Domain.Types.Person.Person, Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> m [API.Types.UI.FollowRide.Followers]
getFollowRide (id, _) = do
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

buildFollower :: Maybe Text -> Maybe Text -> Kernel.Types.Id.Id Booking -> Int -> API.Types.UI.FollowRide.Followers
buildFollower phoneNo name bookingId priority =
  Followers
    { bookingId,
      mobileNumber = fromMaybe "" phoneNo,
      name,
      priority
    }

updateFollowsRideCount :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Kernel.Types.Id.Id Domain.Types.Person.Person -> m ()
updateFollowsRideCount personId = do
  emContacts <- PDEN.findAllByPersonId personId
  let followingContacts = filter (.enableForFollowing) emContacts
  mapM_
    ( \contact -> case contact.contactPersonId of
        Just id -> updateFollowRideCount id
        Nothing -> return ()
    )
    followingContacts
  where
    updateFollowRideCount emPersonId = do
      count <- CQFollowRide.decrementFollowRideCount emPersonId
      when (count <= 0) $ do
        CQFollowRide.clearFollowsRideCounter emPersonId
        QPerson.updateFollowsRide emPersonId False
