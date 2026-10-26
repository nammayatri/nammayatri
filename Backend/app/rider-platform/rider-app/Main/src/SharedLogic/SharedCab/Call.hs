-- | Masked call between a shared-cab rider and the cab's driver. A cab booking has no ride, so the two numbers that ride-hailing carries on the
-- ride come from the app that owns the person: the rider's from here, the driver's from driver-app on demand. Neither is stored.
module SharedLogic.SharedCab.Call
  ( callableSession,
    chatSession,
    maskedCallNumber,
    riderCallNumber,
    driverNumberForRider,
    exophoneCandidates,
    findExophoneByDialled,
  )
where

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Domain.Types.Exophone (Exophone)
import qualified Domain.Types.FRFSTicketBooking as DFTB
import qualified Domain.Types.FRFSTicketBookingStatus as DFRFSBooking
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import Kernel.External.Call.Types (CallService (Exotel))
import Kernel.External.Encryption (decrypt)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.CallBPPInternal as CallBPPInternal
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

-- | The live cab session a booking may call through; Nothing when the city has calls off or the booking is outside the call window.
callableSession ::
  (CacheFlow m r, EsqDBFlow m r, MonadFlow m, Redis.HedisFlow m r) =>
  Config.SharedCabTunables ->
  DFTB.FRFSTicketBooking ->
  m (Maybe Session)
callableSession cfg = liveSession cfg.callEnabled

-- | Same window for chat, under the city's chat switch.
chatSession ::
  (CacheFlow m r, EsqDBFlow m r, MonadFlow m, Redis.HedisFlow m r) =>
  Config.SharedCabTunables ->
  DFTB.FRFSTicketBooking ->
  m (Maybe Session)
chatSession cfg = liveSession cfg.chatEnabled

-- | The cab session of a held seat on the booking's plate that is still running (not ENDED) the trip the rider boarded on.
liveSession ::
  (CacheFlow m r, EsqDBFlow m r, MonadFlow m, Redis.HedisFlow m r) =>
  Bool ->
  DFTB.FRFSTicketBooking ->
  m (Maybe Session)
liveSession enabled booking
  | not enabled || not (isSharedCabBooking booking) = pure Nothing
  | otherwise = do
    tickets <- QFRFSTicket.findAllByTicketBookingIds [booking.id]
    if LegState.callWindow booking.status (map (.status) tickets) booking.vehicleNumber
      then do
        mbSession <- maybe (pure Nothing) Session.readSession booking.vehicleNumber
        pure $ mbSession >>= \s -> if s.status /= ENDED && LegState.tripMatches (getId <$> booking.vehicleTripId) s.vehicleTripId.getId then Just s else Nothing
      else pure Nothing

-- | Exotel reports the dialled ExoPhone with or without its leading zero, and the exophone row stores whichever the city was seeded with
-- (the leg status shows the stored form, which is what the rider dials), so the lookup tries both spellings of the stripped number.
exophoneCandidates :: Text -> [Text]
exophoneCandidates dialled = [stripped, "0" <> stripped]
  where
    stripped = T.dropWhile (== '0') dialled

findExophoneByDialled :: (CacheFlow m r, EsqDBFlow m r) => Text -> m (Maybe Exophone)
findExophoneByDialled dialled = firstJustM CQExophone.findByPhone (exophoneCandidates dialled)

-- | The city's ExoPhone for the rider to dial, picked as ride-hailing does (primary unless it is marked down).
maskedCallNumber :: (CacheFlow m r, EsqDBFlow m r) => Id DMOC.MerchantOperatingCity -> m (Maybe Text)
maskedCallNumber cityId = fmap number . listToMaybe <$> CQExophone.findByMerchantOperatingCityIdAndService cityId Exotel
  where
    number e = if e.isPrimaryDown then e.backupPhone else e.primaryPhone

riderCallNumber :: (EncFlow m r, CacheFlow m r, EsqDBFlow m r) => DFTB.FRFSTicketBooking -> m Text
riderCallNumber booking = do
  person <- QPerson.findById booking.riderId >>= fromMaybeM (PersonNotFound booking.riderId.getId)
  decrypted <- mapM decrypt person.mobileNumber
  fromMaybeM SharedCabCallNumberMissing $ (<>) <$> person.mobileCountryCode <*> decrypted

type CallFlow m r =
  ( CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m,
    Redis.HedisFlow m r,
    Metrics.CoreMetrics m,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    HasRequestId r
  )

-- | The number to put through when the rider dials the ExoPhone: the driver of the rider's newest cab booking that is in the call window.
driverNumberForRider :: CallFlow m r => Id DP.Person -> m (Maybe Text)
driverNumberForRider riderId = do
  bookings <- QFRFSTicketBooking.findAllByRiderId (Just 20) Nothing riderId Nothing Nothing
  firstJustM driverNumberOfBooking [b | b <- bookings, isSharedCabBooking b, b.status == DFRFSBooking.CONFIRMED, isJust b.vehicleNumber]

driverNumberOfBooking :: CallFlow m r => DFTB.FRFSTicketBooking -> m (Maybe Text)
driverNumberOfBooking booking = do
  cfg <- Config.getTunables booking.merchantOperatingCityId
  callableSession cfg booking >>= traverse driverNumberOfSession

driverNumberOfSession :: CallFlow m r => Session -> m Text
driverNumberOfSession s = do
  merchant <- CQM.findById s.merchantId >>= fromMaybeM (MerchantNotFound s.merchantId.getId)
  (.driverNumber) <$> CallBPPInternal.sharedCabDriverCallNumber merchant.driverOfferApiKey merchant.driverOfferBaseUrl s.driverId

firstJustM :: Monad m => (a -> m (Maybe b)) -> [a] -> m (Maybe b)
firstJustM _ [] = pure Nothing
firstJustM f (x : xs) = f x >>= maybe (firstJustM f xs) (pure . Just)
