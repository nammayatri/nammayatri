{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.UI.NyRegularSubscription
  ( postNyRegularSubscriptionsCreate,
    getNyRegularSubscriptionsEstimate,
    -- Make sure to import NyRegularSubscriptionExtra if it's not already
    -- import qualified Storage.Queries.NyRegularSubscriptionExtra as NyRegularSubscriptionExtra
    postNyRegularSubscriptionsConfirm,
    postNyRegularSubscriptionsUpdate,
    getNyRegularSubscriptions,
    getNyRegularSubscriptionDetails,
    postNyRegularSubscriptionsCancel,
  )
where

import qualified API.Types.UI.NyRegularSubscription
import qualified Beckn.ACL.Search as TaxiACL
-- import SharedLogic.Search (SearchReq, OneWaySearchReq)

-- Specific constructors

-- Added import

import Control.Monad (join) -- For joining Maybe (Maybe a)
import Data.Aeson (encode)
import Data.OpenApi (ToSchema)
import qualified Data.Text
import qualified Data.Text as T
import qualified Domain.Action.UI.Quote as Domain.Action.UI.Quote
import qualified Domain.Action.UI.Search as Search
import qualified Domain.Types.Client as Client
import qualified Domain.Types.Estimate
import qualified Domain.Types.Location as Location
import qualified Domain.Types.LocationAddress as LocationAddress
import qualified Domain.Types.Merchant as Domain.Types.Merchant
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.NyRegularSubscription
import qualified Domain.Types.Person as Domain.Types.Person
import qualified Domain.Types.Person as Person
import qualified Domain.Types.SearchRequest as Search
import Environment (Flow)
import qualified Environment
import EulerHS.Prelude hiding (id)
import Kernel.External.Maps (LatLong (..))
import qualified Kernel.Prelude
import Kernel.Types.Error (GenericError (InternalError, InvalidRequest), PersonError (..))
import Kernel.Types.Id (Id)
import qualified Kernel.Types.Id as Id
import qualified Kernel.Types.Version as Kernel.Types.Version
import Kernel.Utils.Common (fork, fromMaybeM, generateGUID, getCurrentTime)
import Kernel.Utils.Logging (logDebug, logInfo)
import Kernel.Utils.Servant.Client (withShortRetry)
import Servant
import qualified SharedLogic.CallBPP as CallBPP
import qualified SharedLogic.Search as Search
import qualified Storage.Queries.NyRegularSubscription as QNyRegularSubscription
import qualified Storage.Queries.NyRegularSubscriptionExtra as NyRegularSubscriptionExtra
import Tools.Auth

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
        Domain.Types.NyRegularSubscription.NyRegularSubscription
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
            status = Domain.Types.NyRegularSubscription.NEW,
            pauseStartDate = Nothing,
            pauseEndDate = Nothing,
            createdAt = now,
            updatedAt = now,
            metadata = req.metadata,
            merchantId = Just merchantId,
            merchantOperatingCityId = Nothing
          }
  void $ QNyRegularSubscription.create newSubscription

  let searchReq = transformToSearchReq req
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

transformToSearchReq :: API.Types.UI.NyRegularSubscription.CreateSubscriptionReq -> Search.SearchReq
transformToSearchReq req =
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
            isReserveRide = Just True
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
    Data.Text.Text ->
    Environment.Flow Domain.Action.UI.Quote.GetQuotesRes
  )
getNyRegularSubscriptionsEstimate = do error "Logic yet to be decided"

postNyRegularSubscriptionsConfirm ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    API.Types.UI.NyRegularSubscription.ConfirmSubscriptionReq ->
    Environment.Flow Domain.Types.NyRegularSubscription.NyRegularSubscription
  )
postNyRegularSubscriptionsConfirm (mPersonId, _) req = do
  personId <- mPersonId & fromMaybeM (PersonNotFound "Person not found in token")
  let subscriptionId = req.subscriptionId

  -- Fetch to verify ownership and existence
  subscription <-
    QNyRegularSubscription.findById subscriptionId
      >>= fromMaybeM (InvalidRequest "Subscription not found") -- Corrected error
  unless (subscription.userId == personId) $
    throwM (InvalidRequest "User does not own this subscription") -- Corrected error

  -- Update status
  QNyRegularSubscription.updateStatusById Domain.Types.NyRegularSubscription.ACTIVE subscriptionId

  -- Fetch and return updated subscription
  QNyRegularSubscription.findById subscriptionId
    >>= fromMaybeM (InvalidRequest "Failed to fetch subscription after status update") -- Corrected error

postNyRegularSubscriptionsUpdate ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    API.Types.UI.NyRegularSubscription.UpdateSubscriptionReq ->
    Environment.Flow Domain.Types.NyRegularSubscription.NyRegularSubscription
  )
postNyRegularSubscriptionsUpdate (mPersonId, _) req = do
  personId <- mPersonId & fromMaybeM (PersonNotFound "Person not found in token")
  let subscriptionIdToUpdate = req.id

  currentSubscription <-
    QNyRegularSubscription.findById subscriptionIdToUpdate
      >>= fromMaybeM (InvalidRequest "Subscription not found for update") -- Corrected error
  unless (currentSubscription.userId == personId) $
    throwM (InvalidRequest "User does not own this subscription for update") -- Corrected error
  now <- getCurrentTime
  let updatedSubscription =
        currentSubscription
          { Domain.Types.NyRegularSubscription.startDatetime = fromMaybe currentSubscription.startDatetime req.startDatetime,
            Domain.Types.NyRegularSubscription.recurrenceRuleDays = fromMaybe currentSubscription.recurrenceRuleDays req.recurrenceRuleDays,
            Domain.Types.NyRegularSubscription.scheduledTimeOfDay = fromMaybe currentSubscription.scheduledTimeOfDay req.scheduledTimeOfDay,
            Domain.Types.NyRegularSubscription.recurrenceEndDate = req.recurrenceEndDate <|> currentSubscription.recurrenceEndDate, -- This is Maybe in domain
            Domain.Types.NyRegularSubscription.status = fromMaybe currentSubscription.status req.status,
            Domain.Types.NyRegularSubscription.pauseStartDate = req.pauseStartDate <|> currentSubscription.pauseStartDate, -- This is Maybe in domain
            Domain.Types.NyRegularSubscription.pauseEndDate = req.pauseEndDate <|> currentSubscription.pauseEndDate, -- This is Maybe in domain
            Domain.Types.NyRegularSubscription.metadata = req.metadata <|> currentSubscription.metadata, -- This is Maybe in domain
            Domain.Types.NyRegularSubscription.updatedAt = now
            -- Note: pickupLocation, dropoffLocation, vehicleServiceTier are not in UpdateSubscriptionReq in the YAML provided earlier.
            -- If they were, they would be updated similarly:
            -- Domain.Types.NyRegularSubscription.pickupLocation = fromMaybe currentSubscription.pickupLocation req.pickupLocation,
            -- Domain.Types.NyRegularSubscription.dropoffLocation = fromMaybe currentSubscription.dropoffLocation req.dropoffLocation,
            -- Domain.Types.NyRegularSubscription.vehicleServiceTier = req.vehicleServiceTier <|> currentSubscription.vehicleServiceTier,
          }

  QNyRegularSubscription.updateByPrimaryKey updatedSubscription
  pure updatedSubscription

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
    Kernel.Types.Id.Id Domain.Types.NyRegularSubscription.NyRegularSubscription ->
    Environment.Flow Domain.Types.NyRegularSubscription.NyRegularSubscription
  )
postNyRegularSubscriptionsCancel = do error "Logic yet to be decided"
