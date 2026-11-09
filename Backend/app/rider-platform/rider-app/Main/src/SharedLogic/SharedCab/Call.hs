-- | Masked call between a shared-cab rider and the cab's driver. A cab booking has no ride, so the two numbers that ride-hailing carries on the
-- ride come from the app that owns the person: the rider's from here, the driver's from driver-app on demand. Neither is stored.
module SharedLogic.SharedCab.Call
  ( liveSession,
    cabExophone,
    maskedCallNumber,
    riderCallNumber,
    personCallNumber,
    CallFlow,
    liveSessionForRider,
    driverNumberOfSession,
    riderCallTarget,
    exophoneCandidates,
    findExophoneByDialled,
  )
where

import Control.Monad.Extra (firstJustM)
import qualified Data.HashMap.Strict as HM
import Data.List (sortOn)
import qualified Data.Text as T
import Domain.Types.Exophone (Exophone)
import qualified Domain.Types.Exophone as Fields
import qualified Domain.Types.FRFSTicketBooking as DFTB
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import Kernel.External.Call.Types (CallService (Exotel))
import Kernel.External.Encryption (decrypt)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Storage.InMem as IM
import qualified Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.CallBPPInternal as CallBPPInternal
import SharedLogic.SharedCab.Allocation.Types (offerPending)
import qualified SharedLogic.SharedCab.BookingState as BookingState
import qualified SharedLogic.SharedCab.Config as Config
import qualified SharedLogic.SharedCab.LegState as LegState
import SharedLogic.SharedCab.RefundDecision (isSharedCabBooking)
import qualified SharedLogic.SharedCab.Session as Session
import SharedLogic.SharedCab.SessionState (Session (..), SessionStatus (..))
import qualified Storage.CachedQueries.Exophone as CQExophone
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.Queries.FRFSTicket as QFRFSTicket
import qualified Storage.Queries.FRFSTicketBooking as QFRFSTicketBooking
import qualified Storage.Queries.Person as QPerson
import Tools.Error
import qualified Tools.Fields as Fields

-- | The cab session of a held seat on the booking's plate that is still running (not ENDED) the trip the rider boarded on. `enabled` is
-- the city's call or chat switch; Nothing when it is off, the booking is outside the call window or its offer is still open.
liveSession ::
  (CacheFlow m r, EsqDBFlow m r, MonadFlow m, Redis.HedisFlow m r) =>
  Bool ->
  DFTB.FRFSTicketBooking ->
  m (Maybe Session)
liveSession enabled booking
  | not enabled || not (isSharedCabBooking booking) = pure Nothing
  | otherwise = do
    tickets <- QFRFSTicket.findAllByTicketBookingIds [booking.id]
    mbAlloc <- BookingState.readAlloc booking.id
    if LegState.callWindow booking.status (map (.status) tickets) booking.vehicleNumber && not (offerPending mbAlloc)
      then do
        mbSession <- maybe (pure Nothing) Session.readSession booking.vehicleNumber
        pure $ mbSession >>= \s -> if s.status /= ENDED && LegState.tripMatches booking.vehicleTripId s.vehicleTripId then Just s else Nothing
      else pure Nothing

-- | Exotel reports the dialled ExoPhone with or without its leading zero, and the exophone row stores whichever the city was seeded with
-- (the leg status shows the stored form, which is what the rider dials), so the lookup tries both spellings of the stripped number.
exophoneCandidates :: Text -> [Text]
exophoneCandidates dialled = [stripped, "0" <> stripped]
  where
    stripped = T.dropWhile (== '0') dialled

findExophoneByDialled :: (CacheFlow m r, EsqDBFlow m r) => Text -> m (Maybe Exophone)
findExophoneByDialled dialled = firstJustM CQExophone.findByPhone (exophoneCandidates dialled)

-- | The city's ExoPhone that cab riders dial. The lowest by number rather than the cache's first, so the webhook can tell it apart from
-- a ride-hailing booking's (randomly picked) ExoPhone whatever order the city's list comes back in.
cabExophone :: (CacheFlow m r, EsqDBFlow m r) => Id DMOC.MerchantOperatingCity -> m (Maybe Exophone)
cabExophone cityId = listToMaybe . sortOn (.primaryPhone) <$> CQExophone.findByMerchantOperatingCityIdAndService cityId Exotel

-- | Picked as ride-hailing does: primary unless it is marked down.
maskedCallNumber :: (CacheFlow m r, EsqDBFlow m r) => Id DMOC.MerchantOperatingCity -> m (Maybe Text)
maskedCallNumber cityId = fmap number <$> cabExophone cityId
  where
    number e = if e.isPrimaryDown then e.backupPhone else e.primaryPhone

-- | Whose driver a rider's call to the ExoPhone reaches: Left the ride-hailing booking, Right the cab. Each comes with whether the
-- dialled ExoPhone is the one it handed out. Both flows draw from the city's same ExoPhones, so the dialled number decides only when
-- it is the taxi's and not the cab's; otherwise a live cab owns the call, since its call window is the short stretch when the rider
-- is at the stop or aboard, and the taxi stays reachable on its own ExoPhone whenever the two differ.
riderCallTarget :: Maybe (ride, Bool) -> Maybe (cab, Bool) -> Maybe (Either ride cab)
riderCallTarget mbRide mbCab = case (mbRide, mbCab) of
  (Just (ride, True), Just (_, False)) -> Just (Left ride)
  (_, Just (cab, _)) -> Just (Right cab)
  (Just (ride, _), Nothing) -> Just (Left ride)
  (Nothing, Nothing) -> Nothing

riderCallNumber :: (EncFlow m r, CacheFlow m r, EsqDBFlow m r) => DFTB.FRFSTicketBooking -> m Text
riderCallNumber booking = do
  person <- QPerson.findById booking.riderId >>= fromMaybeM (PersonNotFound booking.riderId.getId)
  personCallNumber person >>= fromMaybeM SharedCabCallNumberMissing

personCallNumber :: EncFlow m r => DP.Person -> m (Maybe Text)
personCallNumber person = do
  decrypted <- mapM decrypt person.mobileNumber
  pure $ (<>) <$> person.mobileCountryCode <*> decrypted

type CallFlow m r =
  ( CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m,
    Redis.HedisFlow m r,
    Metrics.CoreMetrics m,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    HasRequestId r
  )

-- | The cab whose driver the rider reaches by dialling the ExoPhone: that of the rider's newest cab booking that is in the call window.
liveSessionForRider :: CallFlow m r => Id DP.Person -> m (Maybe Session)
liveSessionForRider riderId = do
  bookings <- QFRFSTicketBooking.findConfirmedSharedCabByRiderId riderId
  firstJustM liveSessionOfBooking [b | b <- bookings, isJust b.vehicleNumber]
  where
    liveSessionOfBooking booking = do
      cfg <- Config.getTunables booking.merchantOperatingCityId
      liveSession cfg.callEnabled booking

-- | Kept in this pod's memory (never Redis) for a few minutes, because a DIRECT-mode rider's leg status asks on every poll. A failed
-- call throws before anything is cached.
driverNumberOfSession :: CallFlow m r => Session -> m Text
driverNumberOfSession s = IM.withInMemCache ["SharedCabDriverNumber", s.driverId] 300 $ do
  merchant <- CQM.findById s.merchantId >>= fromMaybeM (MerchantNotFound s.merchantId.getId)
  (.driverNumber) <$> CallBPPInternal.sharedCabDriverCallNumber merchant.driverOfferApiKey merchant.driverOfferBaseUrl s.driverId
