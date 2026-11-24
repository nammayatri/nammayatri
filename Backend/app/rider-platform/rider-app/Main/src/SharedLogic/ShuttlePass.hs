-- | IO side of the shuttle subscription: the slot's departure on a day, the day's issued ticket, and the OTP.
-- The rules themselves (cutoffs, windows, skip-day merging) are pure, in "SharedLogic.ShuttlePassRules".
module SharedLogic.ShuttlePass
  ( cityTimeDiff,
    slotDepartureOn,
    ticketsForDay,
    recordIssued,
    termLockKey,
    ensureBoardingOtp,
    boardingOtpMatches,
    plainOtp,
    storedBoardingOtp,
  )
where

import BecknV2.FRFS.Utils (frfsVehicleCategoryToBecknVehicleCategory)
import qualified Data.Time as DT
import qualified Domain.Types.BoardingOtp as DBoardingOtp
import qualified Domain.Types.FRFSTicketBooking as DBooking
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Pass as DPass
import qualified Domain.Types.PurchasedPassPayment as DPPP
import qualified Domain.Types.RiderConfig as DRC
import Environment (Flow)
import qualified Environment
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Error (GenericError (..))
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getConfig)
import qualified SharedLogic.External.Nandi.Types as NandiTypes
import qualified SharedLogic.IntegratedBPPConfig as SIBC
import qualified SharedLogic.ShuttlePassRules as Rules
import qualified Storage.CachedQueries.OTPRest.OTPRest as OTPRest
import qualified Storage.CachedQueries.Pass as CQPass
import Storage.ConfigPilot.Config.RiderConfig (RiderConfigDimensions (..))
import qualified Storage.Queries.FRFSTicketBooking as QFRFSTicketBooking
import qualified Storage.Queries.FRFSTicketBookingExtra as QBookingExtra
import System.Random (randomRIO)

cityTimeDiff :: Id DMOC.MerchantOperatingCity -> Flow Seconds
cityTimeDiff cityId = do
  mbRiderConfig <- getConfig (RiderConfigDimensions {merchantOperatingCityId = cityId.getId}) Nothing
  pure $ maybe (Seconds 19800) (\(config :: DRC.RiderConfig) -> config.timeDiffFromUtc) mbRiderConfig

-- | When the term's slot leaves the rider's pickup stop on that day. Nothing when the term has no slot, the
-- timetable does not list the pickup on the trip, or any lookup fails: callers fall back to whole-day rules
-- rather than failing the request.
slotDepartureOn :: Seconds -> DPPP.PurchasedPassPayment -> DT.Day -> Flow (Maybe UTCTime)
slotDepartureOn timeDiff term day = case (term.slotTripId, term.sourceStopCode, term.passId) of
  (Just slotTripId, Just pickupStopCode, Just passId) ->
    withTryCatch "ShuttlePass:slotDepartureOn" (lookupDeparture slotTripId pickupStopCode passId) >>= \case
      Right mbDeparture -> pure mbDeparture
      Left err -> do
        logWarning $ "ShuttlePass: departure lookup failed term=" <> term.id.getId <> " day=" <> show day <> " err=" <> show err
        pure Nothing
  _ -> pure Nothing
  where
    lookupDeparture slotTripId pickupStopCode passId =
      CQPass.findById passId >>= \case
        Nothing -> pure Nothing
        Just (pass :: DPass.Pass) -> do
          integratedBPPConfig <- SIBC.findIntegratedBPPConfig Nothing term.merchantOperatingCityId (frfsVehicleCategoryToBecknVehicleCategory pass.vehicleType) DIBC.MULTIMODAL
          mbTripInfo <- OTPRest.getNandiTripInfo integratedBPPConfig slotTripId
          pickupCodes <- OTPRest.getEquivalentStopCodes pickupStopCode integratedBPPConfig
          pure $ do
            tripInfo <- mbTripInfo
            stop <- find (\(s :: NandiTypes.StopSchedule) -> s.stopCode `elem` pickupCodes) tripInfo.schedule
            pure $ Rules.departureAt timeDiff day stop.departureTime

-- | The term's live tickets for a day. The booking the issue job recorded comes first (it does not depend on
-- the ticket's startTime); otherwise a startTime window around the departure, or around the whole day when the
-- departure is unknown.
ticketsForDay :: Seconds -> DPPP.PurchasedPassPayment -> DT.Day -> Maybe UTCTime -> Flow [DBooking.FRFSTicketBooking]
ticketsForDay timeDiff term day mbDeparture = do
  mbRecorded <- Hedis.safeGet (issuedKey term.id day) :: Flow (Maybe Text)
  recorded <- maybe (pure Nothing) (QFRFSTicketBooking.findById . Id) mbRecorded
  let (from, to) = maybe (Rules.dayTicketWindow timeDiff day) Rules.liveTicketWindow mbDeparture
  windowed <- QBookingExtra.findAllByOverrideAppliedEntityIdAndStartTimeRange term.id.getId from to
  let all' = maybe windowed (\b -> b : filter ((/= b.id) . (.id)) windowed) recorded
  pure $ filter (\(booking :: DBooking.FRFSTicketBooking) -> Rules.isLiveStatus booking.status) all'

-- | Remembers which booking the job issued for a term's day (two days, longer than any issue window).
recordIssued :: DPPP.PurchasedPassPayment -> DT.Day -> Id DBooking.FRFSTicketBooking -> Flow ()
recordIssued term day bookingId = Hedis.setExp (issuedKey term.id day) bookingId.getId (2 * 86400)

issuedKey :: Id DPPP.PurchasedPassPayment -> DT.Day -> Text
issuedKey termId day = "ShuttlePass:issued:" <> termId.getId <> ":" <> show day

-- | The lock both the skip/unskip request and the issue job take for a term, so a day cannot be skipped and
-- ticketed at the same time.
termLockKey :: Id DPPP.PurchasedPassPayment -> Text
termLockKey termId = "ShuttlePass:term:" <> termId.getId

plainOtp :: DBooking.FRFSTicketBooking -> Maybe Text
plainOtp booking = DBoardingOtp.getBoardingOtp <$> booking.boardingOtp

-- | The booking's one boarding OTP. The first caller fixes it in Redis (set-if-absent), so two concurrent runs
-- agree on the same code; it is seeded from the row when the row already has one. The row is (re)written when it
-- differs, which also restores a code that a whole-record booking write replaced with an older read.
ensureBoardingOtp :: DBooking.FRFSTicketBooking -> Flow Text
ensureBoardingOtp booking = do
  let key = otpKey booking.id
  mbCanonical <- Hedis.safeGet key
  otp <- case mbCanonical of
    Just canonical -> pure canonical
    Nothing -> do
      candidate <- maybe (Rules.formatOtp <$> liftIO (randomRIO (0, 9999 :: Int))) pure (plainOtp booking)
      won <- Hedis.setNxExpire key otpTtl candidate
      if won then pure candidate else Hedis.safeGet key >>= fromMaybeM (InternalError "boarding OTP vanished")
  unless (plainOtp booking == Just otp) $
    QFRFSTicketBooking.updateBoardingOtp (Just (DBoardingOtp.BoardingOtp otp)) booking.id
  pure otp

-- | The driver's code against the booking's OTP: the row's value, or the canonical one when the row was overwritten.
boardingOtpMatches :: DBooking.FRFSTicketBooking -> Text -> Flow Bool
boardingOtpMatches booking presented
  | Rules.otpMatches (plainOtp booking) presented = pure True
  | otherwise = (`Rules.otpMatches` presented) <$> storedBoardingOtp booking.id

-- | The canonical OTP the issue job fixed in Redis. Survives a whole-record booking write that reset the row's
-- copy to an older read, until the next job run writes it back.
storedBoardingOtp :: (CacheFlow m r, MonadFlow m) => Id DBooking.FRFSTicketBooking -> m (Maybe Text)
storedBoardingOtp bookingId = Hedis.safeGet (otpKey bookingId)

otpKey :: Id DBooking.FRFSTicketBooking -> Text
otpKey bookingId = "ShuttlePass:otp:" <> bookingId.getId

-- | Three days: longer than any ticket from issue to departure.
otpTtl :: Int
otpTtl = 3 * 86400
