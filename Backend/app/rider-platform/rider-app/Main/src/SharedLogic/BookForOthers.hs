-- | Read-side helpers for rides booked on someone else's behalf ("book for others").
--
-- The booker stays the booking's 'riderId' (they pay and manage the ride). The passenger
-- is linked through 'BookingPartiesLink' with party type @RideParty Passenger@, which is
-- what makes the ride show up as the passenger's active ride in their own app.
--
-- Kept free of heavy imports (no Registration / Notifications) so booking API builders,
-- notifications and call routing can all use it without import cycles.
module SharedLogic.BookForOthers
  ( BookForOthersInfo (..),
    passengerPartyType,
    findActivePassengerLink,
    findPassengerPerson,
    getPassengerMobileNumber,
    mkBookForOthersInfo,
  )
where

import qualified Data.Text as T
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.BookingPartiesLink as DBPL
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Trip as Trip
import Kernel.External.Encryption (decrypt)
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.BookingPartiesLink as QBPL
import qualified Storage.Queries.Person as QP

-- | What the app needs to render a booked-for-others ride, from the viewer's point of view.
data BookForOthersInfo = BookForOthersInfo
  { passengerName :: Text,
    -- | Only shown to the booker; the passenger already knows their own number.
    passengerMobileNumber :: Maybe Text,
    bookedByName :: Maybe Text,
    -- | 'Trip.Initiator' for the booker, 'Trip.RideRolePassenger' for the passenger.
    viewerRole :: Trip.PartyRole
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

passengerPartyType :: Trip.TripParty
passengerPartyType = Trip.RideParty Trip.Passenger

findActivePassengerLink :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Id DRB.Booking -> m (Maybe DBPL.BookingPartiesLink)
findActivePassengerLink bookingId = QBPL.findOneActiveByBookingIdAndTripParty bookingId passengerPartyType

-- | The passenger's person record for a booked-for-others booking, if any.
findPassengerPerson :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Id DRB.Booking -> m (Maybe (DBPL.BookingPartiesLink, DP.Person))
findPassengerPerson bookingId =
  findActivePassengerLink bookingId >>= \case
    Nothing -> pure Nothing
    Just link -> fmap (link,) <$> QP.findById link.partyId

-- | Passenger's phone with country code, e.g. @+919845043210@.
getPassengerMobileNumber :: EncFlow m r => DP.Person -> m (Maybe Text)
getPassengerMobileNumber person = do
  mbNumber <- mapM decrypt person.mobileNumber
  pure $ (fromMaybe "+91" person.mobileCountryCode <>) <$> mbNumber

mkBookForOthersInfo ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r, EncFlow m r) =>
  Id DP.Person ->
  DRB.Booking ->
  m (Maybe BookForOthersInfo)
mkBookForOthersInfo requesterId booking =
  findPassengerPerson booking.id >>= \case
    Nothing -> pure Nothing
    Just (link, passenger) -> do
      let isBooker = requesterId == booking.riderId
      passengerMobileNumber <- if isBooker then getPassengerMobileNumber passenger else pure Nothing
      booker <- QP.findById booking.riderId
      pure $
        Just
          BookForOthersInfo
            { passengerName = link.partyName,
              passengerMobileNumber,
              bookedByName = booker >>= fullName,
              viewerRole = if isBooker then Trip.Initiator else Trip.RideRolePassenger
            }
  where
    fullName person =
      let names = catMaybes [person.firstName, person.lastName]
       in if null names then Nothing else Just (T.intercalate " " names)
