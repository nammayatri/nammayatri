{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.UI.NyRegularSubscription
  ( postNyRegularSubscriptionsCreate,
    getNyRegularSubscriptionsEstimate,
    postNyRegularSubscriptionsConfirm,
    postNyRegularSubscriptionsUpdate,
    getNyRegularSubscriptions,
    getNyRegularSubscriptionDetails,
    postNyRegularSubscriptionsCancel,
  )
where

import qualified API.Types.UI.NyRegularSubscription -- For request/response types
import qualified Beckn.ACL.Search as TaxiACL
import Control.Monad (join, when)
import Data.Aeson (encode, toJSON)
import qualified Data.List as List
import Data.Maybe (fromJust, fromMaybe, isJust)
import Data.OpenApi (ToSchema)
import qualified Data.Text
import qualified Data.Text as T
import qualified Data.Time as Time
import qualified Domain.Action.UI.Quote as Domain.Action.UI.Quote
import qualified Domain.Action.UI.Search as Search
import qualified Domain.Types.Client as Client
import qualified Domain.Types.Estimate
import qualified Domain.Types.Location as Location
import qualified Domain.Types.LocationAddress as LocationAddress
import qualified Domain.Types.Merchant as Domain.Types.Merchant
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.NyRegularInstanceLog as NyRegularInstanceLog
import qualified Domain.Types.NyRegularSubscription
import qualified Domain.Types.NyRegularSubscription as NySub
import qualified Domain.Types.Person as Domain.Types.Person
import qualified Domain.Types.Person as Person
import qualified Domain.Types.SearchRequest as Search
import Environment (Flow)
import qualified Environment
import EulerHS.Prelude hiding (id)
import Kernel.External.Encryption (EncFlow)
import Kernel.External.Maps (LatLong (..))
import qualified Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Error (GenericError (InternalError, InvalidRequest), PersonError (..))
import Kernel.Types.Id (Id (..))
import qualified Kernel.Types.Id as Id
import qualified Kernel.Types.Version as Kernel.Types.Version
import Kernel.Utils.Common (fork, fromMaybeM, generateGUID, getCurrentTime)
import Kernel.Utils.Logging (logDebug, logInfo)
import Kernel.Utils.Servant.Client (withShortRetry)
import qualified Kernel.Utils.Time as KUT
import Lib.Scheduler.JobStorageType.SchedulerType (createJobIn)
import qualified Lib.Scheduler.Types as Scheduler
import Servant
import qualified SharedLogic.CallBPP as CallBPP
import SharedLogic.CallBPPInternal
import SharedLogic.JobScheduler (NyRegularInstanceJobData (..), RiderJobType (NyRegularInstance))
import SharedLogic.NyRegularSubscriptionHasher (calculateSubscriptionSchedulingHash)
import qualified SharedLogic.Search as Search
import Storage.Beam.SchedulerJob ()
import qualified Storage.CachedQueries.Merchant.RiderConfig as QRC
import qualified Storage.Queries.Estimate as QEstimate
import qualified Storage.Queries.Merchant as QMerchant
import qualified Storage.Queries.NyRegularInstanceLog as QNyRegularInstanceLog
import qualified Storage.Queries.NyRegularSubscription as QNyRegularSubscription
import qualified Storage.Queries.NyRegularSubscriptionExtra as NyRegularSubscriptionExtra
import qualified Storage.Queries.PersonExtra as QPerson
import Tools.Auth
import Tools.Error

postNyRegularSubscriptionsCreate ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Prelude.Maybe Data.Text.Text ->
    Kernel.Prelude.Maybe (Kernel.Prelude.Bool) ->
    Kernel.Prelude.Maybe (Kernel.Types.Version.Version) ->
    Kernel.Prelude.Maybe (Kernel.Types.Version.Version) ->
    Kernel.Prelude.Maybe (Kernel.Types.Version.Version) ->
    Kernel.Prelude.Maybe (Data.Text.Text) ->
    Kernel.Prelude.Maybe (Data.Text.Text) ->
    API.Types.UI.NyRegularSubscription.CreateSubscriptionReq ->
    Flow API.Types.UI.NyRegularSubscription.CreateSubscriptionRes
  )
postNyRegularSubscriptionsCreate (mPersonId, merchantId) mbClientId mbIsDashboardRequest mbBundleVersion mbClientVersion mbClientConfigVersion mbRnVersion mbDevice req = do
  personId <- mPersonId & fromMaybeM (PersonNotFound "Person not found")
  person <- QPerson.findByPId personId >>= fromMaybeM (PersonNotFound personId.getId)
  let merchantOperatingCityId = person.merchantOperatingCityId
  now <- getCurrentTime
  subscriptionId <- generateGUID

  let newSubscription =
        NySub.NyRegularSubscription
          { id = subscriptionId,
            userId = personId,
            pickupLocation = req.pickupLocation,
            dropoffLocation = req.dropoffLocation,
            vehicleServiceTier = req.vehicleServiceTier,
            startDatetime = req.startDatetime,
            recurrenceRuleDays = req.recurrenceRuleDays,
            scheduledTimeOfDay = req.scheduledTimeOfDay,
            recurrenceEndDate = req.recurrenceEndDate,
            fixedPrice = Nothing,
            fixedPriceBreakupDetails = Nothing,
            fixedPriceExpiryDate = Nothing,
            initialBppQuoteId = Nothing,
            bppId = req.bppId,
            status = NySub.NEW,
            pauseStartDate = Nothing,
            pauseEndDate = Nothing,
            lastProcessedAt = Nothing,
            createdAt = now,
            updatedAt = now,
            metadata = Nothing,
            merchantId = Just merchantId,
            merchantOperatingCityId = Just merchantOperatingCityId,
            schedulingHash = Nothing -- Initialize with Nothing
          }
  initialHash <- calculateSubscriptionSchedulingHash newSubscription
  let subscriptionWithHash = newSubscription {NySub.schedulingHash = Just $ show initialHash}
  void $ QNyRegularSubscription.createWithLocation subscriptionWithHash

  let searchReq = transformToSearchReq req subscriptionId
  searchRes <-
    Search.search
      personId
      searchReq
      mbBundleVersion
      mbClientVersion
      mbClientConfigVersion
      mbRnVersion
      (Kernel.Types.Id.Id <$> mbClientId)
      mbDevice
      (fromMaybe False mbIsDashboardRequest)
      Nothing
      False
  logInfo $ "New subscription created with id: " <> subscriptionId.getId
  fork "search cabs" . withShortRetry $ do
    becknTaxiReqV2 <- TaxiACL.buildSearchReqV2 searchRes
    let generatedJson = encode becknTaxiReqV2
    logDebug $ "Beckn Taxi Request V2: " <> T.pack (show generatedJson)
    void $ CallBPP.searchV2 searchRes.gatewayUrl becknTaxiReqV2 merchantId

  return $
    API.Types.UI.NyRegularSubscription.CreateSubscriptionRes
      { subscriptionId = subscriptionId,
        searchRequestId = searchRes.searchRequest.id.getId
      }

transformToSearchReq :: API.Types.UI.NyRegularSubscription.CreateSubscriptionReq -> Id NySub.NyRegularSubscription -> Search.SearchReq
transformToSearchReq req subscriptionId =
  let details = req.oneWaySearchReqDetails
   in Search.OneWaySearch
        Search.OneWaySearchReq
          { origin = transformLocation req.pickupLocation,
            destination = Just $ transformLocation req.dropoffLocation,
            startTime = details.startTime,
            stops = Nothing,
            isSourceManuallyMoved = details.isSourceManuallyMoved,
            isDestinationManuallyMoved = details.isDestinationManuallyMoved,
            isReallocationEnabled = details.isReallocationEnabled,
            fareParametersInRateCard = details.fareParametersInRateCard,
            quotesUnifiedFlow = details.quotesUnifiedFlow,
            driverIdentifier = details.driverIdentifier,
            isMeterRideSearch = details.isMeterRideSearch,
            platformType = details.platformType,
            sessionToken = details.sessionToken,
            recentLocationId = details.recentLocationId,
            isSpecialLocation = Nothing,
            placeNameSource = Nothing,
            isReserveRide = Just True,
            subscriptionId = Just subscriptionId
          }
  where
    transformLocation :: Location.Location -> Search.SearchReqLocation
    transformLocation loc =
      Search.SearchReqLocation
        { gps = LatLong {lat = loc.lat, lon = loc.lon},
          address = loc.address
        }

getNyRegularSubscriptionsEstimate ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Data.Text.Text -> -- searchRequestId from path
    -- The API.Action layer will also receive a Maybe (Id NyRegularSubscription) for the query param,
    -- but we are ignoring it here as per instruction.
    Environment.Flow Domain.Action.UI.Quote.GetQuotesRes
  )
getNyRegularSubscriptionsEstimate _auth searchRequestIdText = do
  let searchId = Id.Id searchRequestIdText
  -- The third parameter to getQuotes was for an optional BPPId, not subscriptionId.
  -- Passing Nothing as we don't have a specific BPPId to filter by here.
  Domain.Action.UI.Quote.getQuotes searchId Nothing

postNyRegularSubscriptionsConfirm ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    API.Types.UI.NyRegularSubscription.ConfirmSubscriptionReq ->
    Environment.Flow Domain.Types.NyRegularSubscription.NyRegularSubscription
  )
postNyRegularSubscriptionsConfirm (mPersonId, merchantId) req = do
  personId <- mPersonId & fromMaybeM (PersonNotFound "Person not found in token")
  let subscriptionId = req.subscriptionId
      estimateId = req.estimateId

  -- Fetch to verify ownership and existence
  subscription <-
    QNyRegularSubscription.findById subscriptionId
      >>= fromMaybeM (InvalidRequest "Subscription not found")
  unless (subscription.userId == personId) $
    throwM (InvalidRequest "User does not own this subscription")

  -- Update status
  QNyRegularSubscription.updateStatusById Domain.Types.NyRegularSubscription.ACTIVE subscriptionId

  merchant <- QMerchant.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)

  -- Fetch the estimate by ID
  estimate <- QEstimate.findById estimateId >>= fromMaybeM (EstimateDoesNotExist estimateId.getId)

  estimateDetails <- getEstimateDetails merchant.driverOfferApiKey merchant.driverOfferBaseUrl estimate.bppEstimateId.getId

  -- Update the subscription's metadata field with the BppEstimate as JSON
  let updatedSubscription' = subscription {Domain.Types.NyRegularSubscription.metadata = Just (toJSON estimateDetails)}
  QNyRegularSubscription.updateByPrimaryKey updatedSubscription'

  -- Fetch updated subscription
  let updatedSubscription = updatedSubscription'

  -- Schedule the next instance job
  case updatedSubscription.merchantOperatingCityId of
    Nothing -> do
      logInfo $ "Subscription " <> updatedSubscription.id.getId <> " is missing merchantOperatingCityId, skipping job scheduling"
    Just opCityId -> do
      -- Fetch RiderConfig to get the correct timeDiffFromUtc and execution time offset
      riderConfig <-
        QRC.findByMerchantOperatingCityId opCityId Nothing
          >>= fromMaybeM (RiderConfigDoesNotExist opCityId.getId)

      -- Use UTC for reference time; the offset is only for interpreting scheduledTimeOfDay
      currentTime <- getCurrentTime
      let utcOffset = KUT.secondsToNominalDiffTime riderConfig.timeDiffFromUtc
          localCurrentTime = Time.addUTCTime utcOffset currentTime
          today = Time.utctDay localCurrentTime
          localScheduledTime = Time.UTCTime today (Time.timeOfDayToTime updatedSubscription.scheduledTimeOfDay)
          minGap = KUT.secondsToNominalDiffTime riderConfig.nyRegularMinGapSeconds
      logInfo $ "[NYREGULAR] utcOffset: " <> show utcOffset
      logInfo $ "[NYREGULAR] localCurrentTime: " <> show localCurrentTime
      logInfo $ "[NYREGULAR] localScheduledTime: " <> show localScheduledTime
      logInfo $ "[NYREGULAR] minGap: " <> show minGap
      nextInstanceTimesLocal <- getNextScheduledInstanceTimes minGap localScheduledTime updatedSubscription localCurrentTime

      for_ nextInstanceTimesLocal $ \nextInstanceScheduledTimeLocal -> do
        let jobScheduledTimeLocal = nextInstanceScheduledTimeLocal
            executionTimeOffsetMinutes = fromMaybe 15 (riderConfig.nyRegularExecutionTimeOffsetMinutes)
            jobExecutionBuffer = fromIntegral (- executionTimeOffsetMinutes) * 60 -- Configurable minutes before scheduled time
            jobExecutionTimeLocal = Time.addUTCTime jobExecutionBuffer jobScheduledTimeLocal
            jobExecutionTimeUtc = Time.addUTCTime (-1 * utcOffset) jobExecutionTimeLocal
        logInfo $ "[NYREGULAR] jobScheduledTimeLocal: " <> show jobScheduledTimeLocal
        logInfo $ "[NYREGULAR] jobExecutionTimeLocal: " <> show jobExecutionTimeLocal
        logInfo $ "[NYREGULAR] jobExecutionTimeUtc: " <> show jobExecutionTimeUtc
        jobCreationTime <- getCurrentTime -- current time for scheduleAfter calculation
        logInfo $ "[NYREGULAR] jobCreationTime: " <> show jobCreationTime
        when (jobExecutionTimeUtc <= jobCreationTime) $ do
          throwM (InvalidRequest $ "Job execution time " <> show jobExecutionTimeLocal <> " is not in the future. Current time: " <> show jobCreationTime)
        let scheduleAfter = Time.diffUTCTime jobExecutionTimeUtc jobCreationTime
        logInfo $ "[NYREGULAR] scheduleAfter: " <> show scheduleAfter
        currentHash <- calculateSubscriptionSchedulingHash updatedSubscription
        let jobData =
              NyRegularInstanceJobData
                { nyRegularSubscriptionId = updatedSubscription.id,
                  userId = updatedSubscription.userId,
                  scheduledTime = jobScheduledTimeLocal,
                  expectedSchedulingHash = show currentHash
                }

        -- Log before creating job
        logInfo $
          "Creating NyRegularInstance job for confirmed subscription " <> updatedSubscription.id.getId
            <> " at "
            <> show jobScheduledTimeLocal
            <> " to run in "
            <> show scheduleAfter
            <> " seconds."

        void $
          createJobIn @_ @'NyRegularInstance -- Explicit type application for the job kind
            updatedSubscription.merchantId
            updatedSubscription.merchantOperatingCityId
            scheduleAfter
            jobData
        logInfo $ "Created NyRegularInstance job for confirmed subscription " <> updatedSubscription.id.getId <> " at " <> show jobScheduledTimeLocal

  pure updatedSubscription

-- Helper to check if a UTCTime falls within a pause period [start, end)
isTimestampInPausePeriod :: Time.UTCTime -> Maybe Time.UTCTime -> Maybe Time.UTCTime -> Bool
isTimestampInPausePeriod _ Nothing _ = False
isTimestampInPausePeriod _ _ Nothing = False
isTimestampInPausePeriod ts (Just start) (Just end) = ts >= start && ts < end

-- Helper function to determine if scheduling parameters changed significantly
didSchedulingParametersChange :: NySub.NyRegularSubscription -> NySub.NyRegularSubscription -> Bool
didSchedulingParametersChange oldSub newSub =
  oldSub.schedulingHash /= newSub.schedulingHash

-- Helper function to get the next N scheduled instance times for a subscription
getNextScheduledInstanceTimes ::
  -- | minimum gap required (from riderConfig)
  Time.NominalDiffTime ->
  -- | local scheduled time (UTC + timeDiffFromUtc)
  KUT.UTCTime ->
  -- | the subscription
  NySub.NyRegularSubscription ->
  -- | current time
  KUT.UTCTime ->
  Environment.Flow [KUT.UTCTime]
getNextScheduledInstanceTimes minGap localScheduledTime sub now = do
  logInfo $ "[NYREGULAR] getNextScheduledInstanceTimes called for subscription: " <> sub.id.getId
  logDebug $ "[NYREGULAR] Input parameters - minGap: " <> show minGap <> ", localScheduledTime: " <> show localScheduledTime <> ", now: " <> show now
  logDebug $ "[NYREGULAR] Subscription status: " <> show sub.status
  let today = Time.utctDay now
      dayOfWeekStr = Time.dayOfWeek today
      recurrenceDays = sub.recurrenceRuleDays
      inPause = isTimestampInPausePeriod localScheduledTime sub.pauseStartDate sub.pauseEndDate
  logDebug $ "[NYREGULAR] Calculated values - today: " <> show today <> ", dayOfWeekStr: " <> show dayOfWeekStr
  logDebug $ "[NYREGULAR] Recurrence days: " <> show recurrenceDays <> ", inPause: " <> show inPause

  if dayOfWeekStr `elem` recurrenceDays
    then do
      logDebug $ "[NYREGULAR] Today (" <> show dayOfWeekStr <> ") is in recurrence days"
      let timeWithMinGap = Time.addUTCTime minGap now
          isScheduledTimeAfterMinGap = localScheduledTime > timeWithMinGap

      logDebug $ "[NYREGULAR] Time checks - timeWithMinGap: " <> show timeWithMinGap <> ", isScheduledTimeAfterMinGap: " <> show isScheduledTimeAfterMinGap

      if isScheduledTimeAfterMinGap && not inPause
        then do
          logInfo $ "[NYREGULAR] Returning scheduled time " <> show localScheduledTime <> " for subscription " <> sub.id.getId
          pure [localScheduledTime]
        else do
          logInfo $ "[NYREGULAR] Not returning scheduled time for subscription " <> sub.id.getId <> " - isScheduledTimeAfterMinGap: " <> show isScheduledTimeAfterMinGap <> ", inPause: " <> show inPause
          pure []
    else do
      logInfo $ "[NYREGULAR] Today (" <> show dayOfWeekStr <> ") is not in recurrence days for subscription " <> sub.id.getId
      pure []

postNyRegularSubscriptionsUpdate ::
  ( (Maybe (Id Domain.Types.Person.Person), Id Domain.Types.Merchant.Merchant) ->
    API.Types.UI.NyRegularSubscription.UpdateSubscriptionReq ->
    Flow NySub.NyRegularSubscription
  )
postNyRegularSubscriptionsUpdate (mPersonId, _) req = do
  personId <- mPersonId & fromMaybeM (PersonNotFound "Person not found in token")
  let subscriptionIdToUpdate = req.id

  currentSubscription <-
    QNyRegularSubscription.findById subscriptionIdToUpdate
      >>= fromMaybeM (InvalidRequest "Subscription not found for update")

  unless (currentSubscription.userId == personId) $
    throwM (InvalidRequest "User does not own this subscription for update")

  now <- getCurrentTime
  let updatedSubscriptionInterim =
        currentSubscription
          { NySub.startDatetime = fromMaybe currentSubscription.startDatetime req.startDatetime,
            NySub.recurrenceRuleDays = fromMaybe currentSubscription.recurrenceRuleDays req.recurrenceRuleDays,
            NySub.scheduledTimeOfDay = fromMaybe currentSubscription.scheduledTimeOfDay req.scheduledTimeOfDay,
            NySub.recurrenceEndDate = req.recurrenceEndDate <|> currentSubscription.recurrenceEndDate,
            NySub.status = fromMaybe currentSubscription.status req.status,
            NySub.pauseStartDate = req.pauseStartDate <|> currentSubscription.pauseStartDate,
            NySub.pauseEndDate = req.pauseEndDate <|> currentSubscription.pauseEndDate,
            NySub.metadata = req.metadata <|> currentSubscription.metadata,
            NySub.updatedAt = now
          }

  QNyRegularSubscription.updateByPrimaryKey updatedSubscriptionInterim

  newSchedulingHash <- calculateSubscriptionSchedulingHash updatedSubscriptionInterim
  QNyRegularSubscription.updateSchedulingHashById (Just $ show newSchedulingHash) subscriptionIdToUpdate

  let finalUpdatedSubscription = updatedSubscriptionInterim {NySub.schedulingHash = Just $ show newSchedulingHash}

  let significantChange = didSchedulingParametersChange currentSubscription finalUpdatedSubscription

  when significantChange $ do
    logInfo $ "Significant scheduling change detected for subscription: " <> finalUpdatedSubscription.id.getId

    -- Fetch RiderConfig to get the correct timeDiffFromUtc
    riderConfig <-
      case finalUpdatedSubscription.merchantOperatingCityId of
        Nothing -> throwM $ InvalidRequest "Subscription is missing merchantOperatingCityId, cannot determine local time."
        Just opCityId ->
          QRC.findByMerchantOperatingCityId opCityId Nothing
            >>= fromMaybeM (RiderConfigDoesNotExist opCityId.getId)

    -- Use UTC for reference time; the offset is only for interpreting scheduledTimeOfDay
    currentTime <- getCurrentTime
    let utcOffset = KUT.secondsToNominalDiffTime riderConfig.timeDiffFromUtc
        localCurrentTime = Time.addUTCTime utcOffset currentTime
        today = Time.utctDay localCurrentTime
        localScheduledTime = Time.UTCTime today (Time.timeOfDayToTime finalUpdatedSubscription.scheduledTimeOfDay)
        minGap = KUT.secondsToNominalDiffTime riderConfig.nyRegularMinGapSeconds
    nextInstanceTimesLocal <- getNextScheduledInstanceTimes minGap localScheduledTime finalUpdatedSubscription localCurrentTime

    for_ nextInstanceTimesLocal $ \nextInstanceScheduledTimeLocal -> do
      let nextInstanceScheduledTime = Time.addUTCTime (-1 * utcOffset) nextInstanceScheduledTimeLocal
      let jobDuplicationKey =
            "NyRegularInstanceJobCreationAttempt:"
              <> finalUpdatedSubscription.id.getId
              <> ":"
              <> show newSchedulingHash -- Added hash to the key
              <> ":"
              <> T.pack (Time.formatTime Time.defaultTimeLocale "%Y%m%d%H%M%S" nextInstanceScheduledTime)

      -- Attempt to acquire a lock for this specific instance creation attempt
      lockAcquired <- Hedis.setNxExpire jobDuplicationKey (24 * 60 * 60) True
      logInfo $ "[NYREGULAR] lockAcquired: " <> show lockAcquired
      logInfo $ "[NYREGULAR] jobDuplicationKey: " <> jobDuplicationKey
      logInfo $ "[NYREGULAR] nextInstanceScheduledTimeLocal: " <> show nextInstanceScheduledTimeLocal
      logInfo $ "[NYREGULAR] finalUpdatedSubscription.id: " <> finalUpdatedSubscription.id.getId
      logInfo $ "[NYREGULAR] newSchedulingHash: " <> show newSchedulingHash
      logInfo $ "[NYREGULAR] Time.formatTime Time.defaultTimeLocale \"%Y%m%d%H%M%S\" nextInstanceScheduledTime: " <> T.pack (Time.formatTime Time.defaultTimeLocale "%Y%m%d%H%M%S" nextInstanceScheduledTime)
      logInfo $ "[NYREGULAR] nextInstanceScheduledTime: " <> show nextInstanceScheduledTime
      when lockAcquired $ do
        logInfo $ "Acquired lock for proactive job creation: " <> jobDuplicationKey
        mExistingLog <- QNyRegularInstanceLog.findBySubscriptionIdAndScheduledTime finalUpdatedSubscription.id nextInstanceScheduledTime
        let shouldCreateJob = case mExistingLog of
              Nothing -> True
              Just logEntry ->
                logEntry.automationStatus
                  `elem` [NyRegularInstanceLog.PENDING, NyRegularInstanceLog.FAILED_NO_OFFER, NyRegularInstanceLog.FAILED_BPP_ERROR]
        logInfo $ "[NYREGULAR] shouldCreateJob: " <> show shouldCreateJob
        when shouldCreateJob $ do
          let jobScheduledTime = nextInstanceScheduledTime
              executionTimeOffsetMinutes = fromMaybe 15 (riderConfig.nyRegularExecutionTimeOffsetMinutes)
              jobExecutionBuffer = fromIntegral (- executionTimeOffsetMinutes) * 60 -- Configurable minutes before scheduled time
          let jobExecutionTime = Time.addUTCTime jobExecutionBuffer jobScheduledTime

          jobCreationTime <- getCurrentTime -- current time for scheduleAfter calculation
          when (jobExecutionTime > jobCreationTime) $ do
            -- Ensure job is scheduled for the future
            let scheduleAfter = Time.diffUTCTime jobExecutionTime jobCreationTime
            let jobData =
                  NyRegularInstanceJobData
                    { nyRegularSubscriptionId = finalUpdatedSubscription.id,
                      userId = finalUpdatedSubscription.userId,
                      scheduledTime = jobScheduledTime,
                      expectedSchedulingHash = fromJust finalUpdatedSubscription.schedulingHash -- Safe due to logic above
                    }

            -- Log before creating job
            logInfo $
              "Proactively creating NyRegularInstance job for subscription " <> finalUpdatedSubscription.id.getId
                <> " at "
                <> show jobScheduledTime
                <> " to run in "
                <> show scheduleAfter
                <> " seconds."

            void $
              createJobIn @_ @'NyRegularInstance -- Explicit type application for the job kind
                finalUpdatedSubscription.merchantId
                finalUpdatedSubscription.merchantOperatingCityId
                scheduleAfter
                jobData
            logInfo $ "Proactively created NyRegularInstance job for " <> finalUpdatedSubscription.id.getId <> " at " <> show jobScheduledTime

  pure finalUpdatedSubscription

getNyRegularSubscriptions ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Prelude.Maybe (Domain.Types.NyRegularSubscription.NyRegularSubscriptionStatus) ->
    Kernel.Prelude.Maybe (Kernel.Prelude.Int) ->
    Kernel.Prelude.Maybe (Kernel.Prelude.Int) ->
    Environment.Flow [Domain.Types.NyRegularSubscription.NyRegularSubscription]
  )
getNyRegularSubscriptions (mPersonId, _) mmStatus mmLimit mmOffset = do
  personId <- mPersonId & fromMaybeM (PersonNotFound "Person not found in token")

  let finalStatus = mmStatus -- Flatten Maybe (Maybe Status) to Maybe Status
      finalLimit = mmLimit -- Flatten Maybe (Maybe Int) to Maybe Int
      finalOffsetRaw = mmOffset -- Flatten Maybe (Maybe Int) to Maybe Int - This was the type error source
      finalOffset = fmap fromIntegral finalOffsetRaw -- Corrected: Convert Maybe Int to Maybe Integer
  NyRegularSubscriptionExtra.listSubscriptionsByFilters personId finalStatus finalLimit finalOffset

getNyRegularSubscriptionDetails ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.NyRegularSubscription.NyRegularSubscription ->
    Environment.Flow Domain.Types.NyRegularSubscription.NyRegularSubscription
  )
getNyRegularSubscriptionDetails (mPersonId, _) subscriptionId = do
  personId <- mPersonId & fromMaybeM (PersonNotFound "Person not found in token")
  subscription <-
    QNyRegularSubscription.findById subscriptionId
      >>= fromMaybeM (InvalidRequest "Subscription not found") -- Corrected error
  unless (subscription.userId == personId) $
    throwM (InvalidRequest "User does not own this subscription") -- Corrected error
  pure subscription

postNyRegularSubscriptionsCancel ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.NyRegularSubscription.NyRegularSubscription -> -- subscriptionId from path
    Environment.Flow Domain.Types.NyRegularSubscription.NyRegularSubscription
  )
postNyRegularSubscriptionsCancel (mPersonId, _) subscriptionIdToCancel = do
  personId <- mPersonId & fromMaybeM (PersonNotFound "Person not found in token")

  -- Fetch to verify ownership and existence
  subscription <-
    QNyRegularSubscription.findById subscriptionIdToCancel
      >>= fromMaybeM (InvalidRequest "Subscription not found to cancel")

  unless (Domain.Types.NyRegularSubscription.userId subscription == personId) $
    throwM (InvalidRequest "User does not own this subscription to cancel")

  -- Update status to CANCELLED
  QNyRegularSubscription.updateStatusById Domain.Types.NyRegularSubscription.CANCELLED subscriptionIdToCancel

  -- Fetch and return updated subscription
  QNyRegularSubscription.findById subscriptionIdToCancel
    >>= fromMaybeM (InvalidRequest "Failed to fetch subscription after cancellation")
