-- | Write-side of "book for others": resolving the passenger a rider books for and
-- linking them to the search request. The link is copied to the booking on confirm
-- (see 'SharedLogic.Confirm'), from where notifications, SMS, calls and the passenger's
-- own app pick it up.
module Domain.Action.UI.BookForOthers
  ( ResolvedPassenger (..),
    validatePassengerDetails,
    resolvePassenger,
    setSearchPassenger,
    clearSearchPassenger,
  )
where

import qualified Domain.Action.UI.Registration as Reg
import Domain.Types.PassengerDetails
import qualified Domain.Types.Person as DP
import qualified Domain.Types.SearchRequest as DSR
import qualified Domain.Types.SearchRequestPartiesLink as DSRPL
import Kernel.Prelude
import Kernel.Storage.Clickhouse.Config (ClickhouseFlow)
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Tools.Metrics.CoreMetrics.Types (DeploymentVersion)
import Kernel.Types.Id
import Kernel.Types.Predicate (LengthInRange (..))
import Kernel.Types.Version (CloudType)
import Kernel.Utils.Common
import qualified Kernel.Utils.Predicates as P
import Kernel.Utils.Validation
import qualified SharedLogic.BookForOthers as SBFO
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.SearchRequestPartiesLink as QSRPL
import Tools.Error

data ResolvedPassenger = ResolvedPassenger
  { personId :: Id DP.Person,
    name :: Text
  }

validatePassengerDetails :: Validate PassengerDetails
validatePassengerDetails PassengerDetails {..} =
  sequenceA_
    [ validateField "name" name $ LengthInRange 1 100,
      validateField "phoneNumber" phoneNumber P.mobileNumber,
      whenJust countryCode $ \cc -> validateField "countryCode" cc P.mobileCountryCode
    ]

-- | Validates the passenger and returns their person id. When the number has no account yet
-- a placeholder one is created (same as delivery receivers), so the ride shows up in their app
-- if they sign up with that number.
resolvePassenger ::
  ( HasFlowEnv m r '["version" ::: DeploymentVersion, "cloudType" ::: Maybe CloudType],
    EncFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    CacheFlow m r,
    ClickhouseFlow m r
  ) =>
  DP.Person ->
  PassengerDetails ->
  m ResolvedPassenger
resolvePassenger booker req = do
  runRequestValidation validatePassengerDetails req
  passengerId <- Reg.createPersonWithPhoneNumber booker.merchantId req.phoneNumber req.countryCode
  when (passengerId == booker.id) $ throwError (InvalidRequest "PASSENGER_CANNOT_BE_SELF")
  -- Same rule as delivery parties: one active ride per person, so the passenger's app
  -- never has two rides to track.
  hasActiveBooking <- isJust <$> QBooking.findLatestSelfAndPartyBookingByRiderId passengerId
  when hasActiveBooking $ throwError (InvalidRequest "ACTIVE_BOOKING_PRESENT_FOR_PASSENGER")
  pure ResolvedPassenger {personId = passengerId, name = req.name}

-- | Links the passenger to the search, replacing any passenger set earlier for it.
setSearchPassenger :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Id DSR.SearchRequest -> ResolvedPassenger -> m ()
setSearchPassenger searchRequestId passenger = do
  clearSearchPassenger searchRequestId
  linkId <- generateGUID
  now <- getCurrentTime
  QSRPL.create
    DSRPL.SearchRequestPartiesLink
      { id = Id linkId,
        searchRequestId,
        partyId = passenger.personId,
        partyName = passenger.name,
        partyType = SBFO.passengerPartyType,
        createdAt = now,
        updatedAt = now
      }

-- | Back to booking for oneself.
clearSearchPassenger :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Id DSR.SearchRequest -> m ()
clearSearchPassenger searchRequestId = QSRPL.deleteBySearchRequestIdAndPartyType searchRequestId SBFO.passengerPartyType
