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
import qualified Storage.Queries.Merchant as QMerchant
import qualified Storage.Queries.NyRegularInstanceLog as QNyRegularInstanceLog
import qualified Storage.Queries.NyRegularSubscription as QNyRegularSubscription
import qualified Storage.Queries.NyRegularSubscriptionExtra as NyRegularSubscriptionExtra
import Tools.Auth
import Tools.Error

postNyRegularSubscriptionsCreate ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Prelude.Maybe (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Client.Client)) ->
    Kernel.Prelude.Maybe (Kernel.Prelude.Bool) ->
    Kernel.Prelude.Maybe (Kernel.Prelude.Maybe Kernel.Types.Version.Version) ->
    Kernel.Prelude.Maybe (Kernel.Prelude.Maybe Kernel.Types.Version.Version) ->
    Kernel.Prelude.Maybe (Kernel.Prelude.Maybe Kernel.Types.Version.Version) ->
    Kernel.Prelude.Maybe (Kernel.Prelude.Maybe Data.Text.Text) ->
    Kernel.Prelude.Maybe (Kernel.Prelude.Maybe Data.Text.Text) ->
    API.Types.UI.NyRegularSubscription.CreateSubscriptionReq ->
    Flow API.Types.UI.NyRegularSubscription.CreateSubscriptionRes
  )
postNyRegularSubscriptionsCreate (mPersonId, merchantId) mbClientId mbIsDashboardRequest mbBundleVersion mbClientVersion mbClientConfigVersion mbRnVersion mbDevice req = do
  personId <- mPersonId & fromMaybeM (PersonNotFound "Person not found")
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
            merchantOperatingCityId = Nothing,
            schedulingHash = Nothing -- Initialize with Nothing
          }
  initialHash <- calculateSubscriptionSchedulingHash newSubscription
  let subscriptionWithHash = newSubscription {NySub.schedulingHash = Just initialHash}
  void $ QNyRegularSubscription.create subscriptionWithHash

  let searchReq = transformToSearchReq req subscriptionId
  searchRes <-
    Search.search
      personId
      searchReq
      (join mbBundleVersion)
      (join mbClientVersion)
      (join mbClientConfigVersion)
      (join mbRnVersion)
      (join mbClientId)
      (join mbDevice)
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

  -- Call BPP internal API to get the estimate
  estimateDetails <- getEstimateDetails estimateId.getId merchant.driverOfferBaseUrl merchant.driverOfferApiKey

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

      -- Get the next scheduled instance time
      -- Calculate the difference between current time and the next scheduled time
      let today = Time.utctDay currentTime
          naiveUTCTime = Time.UTCTime today (Time.timeOfDayToTime updatedSubscription.scheduledTimeOfDay)
          nextScheduledTime = Time.addUTCTime (KUT.secondsToNominalDiffTime riderConfig.timeDiffFromUtc) naiveUTCTime
          timeDiff = Time.diffUTCTime nextScheduledTime currentTime
      nextInstanceTimes <- getNextScheduledInstanceTimes (min timeDiff (KUT.secondsToNominalDiffTime riderConfig.timeDiffFromUtc)) updatedSubscription currentTime 1

      for_ nextInstanceTimes $ \nextInstanceScheduledTime -> do
        let jobScheduledTime = nextInstanceScheduledTime
            executionTimeOffsetMinutes = fromMaybe 15 (riderConfig.nyRegularExecutionTimeOffsetMinutes)
            jobExecutionBuffer = fromIntegral (- executionTimeOffsetMinutes) * 60 -- Configurable minutes before scheduled time
        let jobExecutionTime = Time.addUTCTime jobExecutionBuffer jobScheduledTime

        jobCreationTime <- getCurrentTime -- current time for scheduleAfter calculation
        when (jobExecutionTime <= jobCreationTime) $ do
          throwM (InvalidRequest $ "Job execution time " <> show jobExecutionTime <> " is not in the future. Current time: " <> show jobCreationTime)
        -- Ensure job is scheduled for the future
        let scheduleAfter = Time.diffUTCTime jobExecutionTime jobCreationTime
        -- Calculate the current scheduling hash
        currentHash <- calculateSubscriptionSchedulingHash updatedSubscription
        let jobData =
              NyRegularInstanceJobData
                { nyRegularSubscriptionId = updatedSubscription.id,
                  userId = updatedSubscription.userId,
                  scheduledTime = jobScheduledTime,
                  expectedSchedulingHash = currentHash
                }

        -- Log before creating job
        logInfo $
          "Creating NyRegularInstance job for confirmed subscription " <> updatedSubscription.id.getId
            <> " at "
            <> show jobScheduledTime
            <> " to run in "
            <> show scheduleAfter
            <> " seconds."

        void $
          createJobIn @_ @'NyRegularInstance -- Explicit type application for the job kind
            updatedSubscription.merchantId
            updatedSubscription.merchantOperatingCityId
            scheduleAfter
            jobData
        logInfo $ "Created NyRegularInstance job for confirmed subscription " <> updatedSubscription.id.getId <> " at " <> show jobScheduledTime

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
  Time.NominalDiffTime -> -- The timeDiffFromUtc from RiderConfig
  NySub.NyRegularSubscription ->
  Time.UTCTime ->
  Int ->
  Flow [Time.UTCTime]
getNextScheduledInstanceTimes timeDiffFromUtc sub referenceTime maxInstancesToFind
  | maxInstancesToFind <= 0 = pure []
  | sub.status /= NySub.ACTIVE = pure []
  | otherwise = do
    let subStartDay = Time.utctDay sub.startDatetime
        referenceDay = Time.utctDay referenceTime
        initialSearchDay = max subStartDay referenceDay
        iterationLimitDays = 365 * 2
    loop initialSearchDay 0 [] iterationLimitDays
  where
    loop :: Time.Day -> Int -> [Time.UTCTime] -> Int -> Flow [Time.UTCTime]
    loop currentDay foundCount accInstances daysLeft
      | foundCount >= maxInstancesToFind = pure (List.reverse accInstances)
      | daysLeft <= 0 = pure (List.reverse accInstances)
      | otherwise = do
        let pastRecurrenceEnd = case sub.recurrenceEndDate of
              Just endDate -> currentDay > endDate
              Nothing -> False

        if pastRecurrenceEnd
          then pure (List.reverse accInstances)
          else do
            let currentDayOfWeek = Time.dayOfWeek currentDay
            if currentDayOfWeek `elem` sub.recurrenceRuleDays
              then do
                -- Create a naive UTC time, then subtract the offset to get the true UTC time
                let naiveUTCTime = Time.UTCTime currentDay (Time.timeOfDayToTime sub.scheduledTimeOfDay)
                let potentialInstanceTime = Time.addUTCTime timeDiffFromUtc naiveUTCTime

                if potentialInstanceTime > referenceTime
                  && not (isTimestampInPausePeriod potentialInstanceTime sub.pauseStartDate sub.pauseEndDate)
                  then loop (Time.addDays 1 currentDay) (foundCount + 1) (potentialInstanceTime : accInstances) (daysLeft - 1)
                  else loop (Time.addDays 1 currentDay) foundCount accInstances (daysLeft - 1)
              else loop (Time.addDays 1 currentDay) foundCount accInstances (daysLeft - 1)

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
  QNyRegularSubscription.updateSchedulingHashById (Just newSchedulingHash) subscriptionIdToUpdate

  let finalUpdatedSubscription = updatedSubscriptionInterim {NySub.schedulingHash = Just newSchedulingHash}

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

    -- Pass the offset to the helper
    nextInstanceTimes <- getNextScheduledInstanceTimes (KUT.secondsToNominalDiffTime riderConfig.timeDiffFromUtc) finalUpdatedSubscription currentTime 1

    for_ nextInstanceTimes $ \nextInstanceScheduledTime -> do
      let jobDuplicationKey =
            "NyRegularInstanceJobCreationAttempt:"
              <> finalUpdatedSubscription.id.getId
              <> ":"
              <> show newSchedulingHash -- Added hash to the key
              <> ":"
              <> T.pack (Time.formatTime Time.defaultTimeLocale "%Y%m%d%H%M%S" nextInstanceScheduledTime)

      -- Attempt to acquire a lock for this specific instance creation attempt
      lockAcquired <- Hedis.setNxExpire jobDuplicationKey (24 * 60 * 60) True

      when lockAcquired $ do
        logInfo $ "Acquired lock for proactive job creation: " <> jobDuplicationKey
        mExistingLog <- QNyRegularInstanceLog.findBySubscriptionIdAndScheduledTime finalUpdatedSubscription.id nextInstanceScheduledTime
        let shouldCreateJob = case mExistingLog of
              Nothing -> True
              Just logEntry ->
                logEntry.automationStatus
                  `elem` [NyRegularInstanceLog.PENDING, NyRegularInstanceLog.FAILED_NO_OFFER, NyRegularInstanceLog.FAILED_BPP_ERROR]

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
    Kernel.Prelude.Maybe (Kernel.Prelude.Maybe Domain.Types.NyRegularSubscription.NyRegularSubscriptionStatus) ->
    Kernel.Prelude.Maybe (Kernel.Prelude.Maybe Kernel.Prelude.Int) ->
    Kernel.Prelude.Maybe (Kernel.Prelude.Maybe Kernel.Prelude.Int) ->
    Environment.Flow [Domain.Types.NyRegularSubscription.NyRegularSubscription]
  )
getNyRegularSubscriptions (mPersonId, _) mmStatus mmLimit mmOffset = do
  personId <- mPersonId & fromMaybeM (PersonNotFound "Person not found in token")

  let finalStatus = join mmStatus -- Flatten Maybe (Maybe Status) to Maybe Status
      finalLimit = join mmLimit -- Flatten Maybe (Maybe Int) to Maybe Int
      finalOffsetRaw = join mmOffset -- Flatten Maybe (Maybe Int) to Maybe Int - This was the type error source
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
