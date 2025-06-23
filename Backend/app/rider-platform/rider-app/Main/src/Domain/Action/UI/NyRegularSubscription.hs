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

import qualified API.Types.UI.NyRegularSubscription
import qualified Beckn.ACL.Search as TaxiACL
-- import SharedLogic.Search (SearchReq, OneWaySearchReq)

import Data.Aeson (encode)
import Data.OpenApi (ToSchema)
import qualified Data.Text
import qualified Data.Text as T
import qualified Domain.Action.UI.Search as Search
import qualified Domain.Types.Client as Client
import qualified Domain.Types.Estimate
import qualified Domain.Types.Location as Location
import qualified Domain.Types.LocationAddress as LocationAddress
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.NyRegularSubscription
import qualified Domain.Types.Person as Person
import qualified Domain.Types.SearchRequest as Search
import Environment (Flow)
import qualified Environment
import EulerHS.Prelude hiding (id)
import Kernel.External.Maps (LatLong (..))
import qualified Kernel.Prelude
import Kernel.Types.Error (PersonError (PersonNotFound))
import Kernel.Types.Id (Id)
import qualified Kernel.Types.Id as Id
import qualified Kernel.Types.Version as Version
import Kernel.Utils.Common (fork, fromMaybeM, generateGUID, getCurrentTime)
import Kernel.Utils.Logging (logDebug, logInfo)
import Kernel.Utils.Servant.Client (withShortRetry)
import Servant
import qualified SharedLogic.CallBPP as CallBPP
import qualified SharedLogic.Search as Search
import qualified Storage.Queries.NyRegularSubscription as QNyRegularSubscription
import Tools.Auth

postNyRegularSubscriptionsCreate ::
  ( (Maybe (Id Person.Person), Id Merchant.Merchant) ->
    Maybe (Maybe (Id Client.Client)) ->
    Maybe Bool ->
    Maybe (Maybe Version.Version) ->
    Maybe (Maybe Version.Version) ->
    Maybe (Maybe Version.Version) ->
    Maybe (Maybe Text) ->
    Maybe (Maybe Text) ->
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
  ( ( Maybe (Id.Id Person.Person),
      Id.Id Merchant.Merchant
    ) ->
    Data.Text.Text ->
    Environment.Flow [Domain.Types.Estimate.Estimate]
  )
getNyRegularSubscriptionsEstimate = do error "Logic yet to be decided"

postNyRegularSubscriptionsConfirm ::
  ( ( Maybe (Id.Id Person.Person),
      Id.Id Merchant.Merchant
    ) ->
    API.Types.UI.NyRegularSubscription.ConfirmSubscriptionReq ->
    Environment.Flow Domain.Types.NyRegularSubscription.NyRegularSubscription
  )
postNyRegularSubscriptionsConfirm = do error "Logic yet to be decided"

postNyRegularSubscriptionsUpdate ::
  ( ( Maybe (Id.Id Person.Person),
      Id.Id Merchant.Merchant
    ) ->
    API.Types.UI.NyRegularSubscription.UpdateSubscriptionReq ->
    Environment.Flow Domain.Types.NyRegularSubscription.NyRegularSubscription
  )
postNyRegularSubscriptionsUpdate = do error "Logic yet to be decided"

getNyRegularSubscriptions ::
  ( ( Maybe (Id.Id Person.Person),
      Id.Id Merchant.Merchant
    ) ->
    Environment.Flow [Domain.Types.NyRegularSubscription.NyRegularSubscription]
  )
getNyRegularSubscriptions = do error "Logic yet to be decided"

getNyRegularSubscriptionDetails ::
  ( ( Maybe (Id.Id Person.Person),
      Id.Id Merchant.Merchant
    ) ->
    Id.Id Domain.Types.NyRegularSubscription.NyRegularSubscription ->
    Environment.Flow Domain.Types.NyRegularSubscription.NyRegularSubscription
  )
getNyRegularSubscriptionDetails = do error "Logic yet to be decided"

postNyRegularSubscriptionsCancel ::
  ( ( Maybe (Id.Id Person.Person),
      Id.Id Merchant.Merchant
    ) ->
    Id.Id Domain.Types.NyRegularSubscription.NyRegularSubscription ->
    Environment.Flow Domain.Types.NyRegularSubscription.NyRegularSubscription
  )
postNyRegularSubscriptionsCancel = do error "Logic yet to be decided"
