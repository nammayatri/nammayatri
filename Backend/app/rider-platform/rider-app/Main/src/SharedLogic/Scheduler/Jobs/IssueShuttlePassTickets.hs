-- | Shuttle subscription: issues each rider's ticket for the day at the change cutoff, 5 hours before their
-- slot departs. After that point reschedule, skip and cancel are closed, so the ticket no longer moves.
--
-- Runs every 15 minutes per city. Each run looks at terms covering today and tomorrow (a slot before 05:00
-- is issued the evening before) and books, through pass instant-book, every slot whose cutoff has passed and
-- whose departure has not, unless the day is skipped or already has a live ticket. Every live ticket gets a
-- boarding OTP, and an hour before departure the rider is sent it with the driver and vehicle (held back until
-- a vehicle is assigned, but never later than 15 minutes before departure).
--
-- Runs on the rider-app environment (not the scheduler's) because instant-book is an app-level flow.
module SharedLogic.Scheduler.Jobs.IssueShuttlePassTickets
  ( issueShuttlePassTickets,
  )
where

import qualified API.Types.UI.FRFSTicketService as FRFSTicketService
import qualified Data.Time as DT
import qualified Domain.Action.UI.FRFSTicketService as DFRFSTicketService
import qualified Domain.Types.FRFSTicketBooking as DBooking
import qualified Domain.Types.PurchasedPassPayment as DPPP
import Environment (Flow)
import qualified Environment
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Error (GenericError (..))
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Scheduler
import Lib.Scheduler.JobStorageType.SchedulerType (createJobIn)
import SharedLogic.JobScheduler
import qualified SharedLogic.ShuttlePass as ShuttlePass
import Storage.Beam.SchedulerJob ()
import qualified Storage.Queries.FRFSTicketBooking as QFRFSTicketBooking
import qualified Storage.Queries.PurchasedPassPaymentExtra as QPPPExtra
import qualified Tools.Notifications as Notify

issueShuttlePassTickets :: Job 'IssueShuttlePassTickets -> Flow ExecutionResult
issueShuttlePassTickets Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) $ do
  let jobData = jobInfo.jobData
      cityId = jobData.merchantOperatingCityId
  timeDiff <- ShuttlePass.cityTimeDiff cityId
  now <- getCurrentTime
  let today = ShuttlePass.localDay timeDiff now
  issued <- sum <$> mapM (issueForDay timeDiff now cityId.getId) [today, DT.addDays 1 today]
  logInfo $ "IssueShuttlePassTickets: issued=" <> show issued
  when jobData.autoSchedule $ scheduleNext jobData
  pure Complete

issueForDay :: Seconds -> UTCTime -> Text -> DT.Day -> Flow Int
issueForDay timeDiff now cityId day = go 0 0
  where
    pageSize = 500
    go offset count = do
      terms <- QPPPExtra.findActiveShuttleTermsOnDay cityId day pageSize offset
      results <- forM terms $ \term ->
        withTryCatch "IssueShuttlePassTickets:term" (issueForTerm timeDiff now term day) >>= \case
          Right issuedOne -> pure issuedOne
          Left err -> do
            logError $ "IssueShuttlePassTickets: could not issue term=" <> term.id.getId <> " day=" <> show day <> " err=" <> show err
            pure False
      let count' = count + length (filter identity results)
      if length terms < pageSize then pure count' else go (offset + pageSize) count'

-- | True when a ticket was issued for this term and day.
issueForTerm :: Seconds -> UTCTime -> DPPP.PurchasedPassPayment -> DT.Day -> Flow Bool
issueForTerm timeDiff now term day
  | day `elem` map (.day) term.skipDays = pure False
  | otherwise =
    ShuttlePass.slotDepartureOn timeDiff term day >>= \case
      Nothing -> do
        logWarning $ "IssueShuttlePassTickets: no departure for term=" <> term.id.getId <> " slot=" <> show term.slotTripId <> " day=" <> show day
        pure False
      Just departure
        | now < ShuttlePass.issueTimeOn departure || now >= departure -> pure False
        | otherwise -> do
          existing <- ShuttlePass.liveTickets term departure
          issued <- if null existing then book term >> pure True else pure False
          tickets <- if issued then ShuttlePass.liveTickets term departure else pure existing
          withOtps <- mapM ensureOtp tickets
          when (now >= addUTCTime (negate ShuttlePass.otpLeadTime) departure) $
            forM_ withOtps $ \(booking, otp) -> sendOtpOnce timeDiff now departure booking otp
          pure issued

-- | Every live ticket carries an OTP, including one issued outside this job (a retry or a rescheduled child).
ensureOtp :: DBooking.FRFSTicketBooking -> Flow (DBooking.FRFSTicketBooking, Text)
ensureOtp booking = case booking.boardingOtp of
  Just otp -> pure (booking, otp)
  Nothing -> do
    otp <- ShuttlePass.newBoardingOtp
    QFRFSTicketBooking.updateBoardingOtp (Just otp) booking.id
    pure (booking, otp)

-- | Sent once per ticket. Waits for a vehicle while there is time; 15 minutes before departure it goes anyway.
sendOtpOnce :: Seconds -> UTCTime -> UTCTime -> DBooking.FRFSTicketBooking -> Text -> Flow ()
sendOtpOnce timeDiff now departure booking otp = do
  let lastCall = now >= addUTCTime (negate 900) departure
  when (isJust booking.vehicleNumber || lastCall) $ do
    firstTime <- Hedis.tryLockRedis ("IssueShuttlePassTickets:otpSent:" <> booking.id.getId) 86400
    when firstTime $ do
      Notify.notifyShuttleBoardingOtp timeDiff departure otp booking
      logInfo $ "IssueShuttlePassTickets: sent OTP booking=" <> booking.id.getId <> " vehicle=" <> show booking.vehicleNumber

book :: DPPP.PurchasedPassPayment -> Flow ()
book term = do
  sourceStopCode <- term.sourceStopCode & fromMaybeM (InvalidRequest "Term has no source stop")
  destinationStopCode <- term.destinationStopCode & fromMaybeM (InvalidRequest "Term has no destination stop")
  let req =
        FRFSTicketService.FRFSPassInstantBookReq
          { purchasedPassPaymentId = term.id,
            sourceStopCode,
            destinationStopCode,
            quantity = Just 1,
            serviceTier = Nothing,
            routeCode = Nothing,
            vehicleNumber = Nothing,
            tripId = term.slotTripId,
            seatIds = Nothing,
            platformType = Nothing
          }
  -- forceBook: a recent ticket on the same pass (e.g. an ad-hoc ride) must not block the daily ticket.
  resp <- DFRFSTicketService.postFrfsPassInstantBook (Just term.personId, term.merchantId) (Just True) req
  logInfo $ "IssueShuttlePassTickets: issued term=" <> term.id.getId <> " booking=" <> show ((.bookingId) <$> resp.booking) <> " inProgress=" <> show resp.inProgress

-- | One next run per city per 15 minutes, however many times this run is triggered.
scheduleNext :: IssueShuttlePassTicketsJobData -> Flow ()
scheduleNext jobData = do
  let interval = 900 :: Int
      lockKey = "IssueShuttlePassTickets:nextRun:" <> jobData.merchantOperatingCityId.getId
  claimed <- Hedis.tryLockRedis lockKey (interval - 60)
  when claimed $
    createJobIn @_ @'IssueShuttlePassTickets (Just jobData.merchantId) (Just jobData.merchantOperatingCityId) (fromIntegral interval) jobData
