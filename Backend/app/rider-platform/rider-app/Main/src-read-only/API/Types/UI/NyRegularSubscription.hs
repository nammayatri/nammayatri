{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.NyRegularSubscription where

import qualified Data.Aeson
import Data.OpenApi (ToSchema)
import qualified Data.Text
import qualified Data.Time
import qualified Data.Time.Calendar
import qualified Data.Time.LocalTime
import qualified Domain.Types.Estimate
import qualified Domain.Types.IntegratedBPPConfig
import qualified Domain.Types.NyRegularSubscription
import qualified Domain.Types.Person
import qualified Domain.Types.RecentLocation
import qualified Domain.Types.RefereeLink
import qualified Domain.Types.ServiceTierType
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Kernel.Types.Price
import Servant
import qualified SharedLogic.Search
import Tools.Auth

data ConfirmSubscriptionReq = ConfirmSubscriptionReq {estimateId :: Kernel.Types.Id.Id Domain.Types.Estimate.Estimate, subscriptionId :: Kernel.Types.Id.Id Domain.Types.NyRegularSubscription.NyRegularSubscription}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data CreateSubscriptionReq = CreateSubscriptionReq
  { bppId :: Data.Text.Text,
    dropoffLocation :: SharedLogic.Search.SearchReqLocation,
    metadata :: Kernel.Prelude.Maybe Data.Aeson.Value,
    oneWaySearchReqDetails :: OneWaySearchReqDetails,
    pickupLocation :: SharedLogic.Search.SearchReqLocation,
    recurrenceEndDate :: Kernel.Prelude.Maybe Data.Time.Calendar.Day,
    recurrenceRuleDays :: [Data.Time.Calendar.DayOfWeek],
    scheduledTimeOfDay :: Data.Time.LocalTime.TimeOfDay,
    startDatetime :: Data.Time.UTCTime,
    vehicleServiceTier :: Kernel.Prelude.Maybe Domain.Types.ServiceTierType.ServiceTierType
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data CreateSubscriptionRes = CreateSubscriptionRes {searchRequestId :: Data.Text.Text, subscriptionId :: Kernel.Types.Id.Id Domain.Types.NyRegularSubscription.NyRegularSubscription}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data NyRegularSubscriptionApiEntity = NyRegularSubscriptionApiEntity
  { bppId :: Data.Text.Text,
    createdAt :: Data.Time.UTCTime,
    dropoffLocation :: SharedLogic.Search.SearchReqLocation,
    fixedPrice :: Kernel.Prelude.Maybe Kernel.Types.Price.Price,
    fixedPriceBreakupDetails :: Kernel.Prelude.Maybe Data.Aeson.Value,
    fixedPriceExpiryDate :: Kernel.Prelude.Maybe Data.Time.UTCTime,
    id :: Kernel.Types.Id.Id Domain.Types.NyRegularSubscription.NyRegularSubscription,
    initialBppQuoteId :: Kernel.Prelude.Maybe Data.Text.Text,
    lastProcessedAt :: Kernel.Prelude.Maybe Data.Time.UTCTime,
    metadata :: Kernel.Prelude.Maybe Data.Text.Text,
    nextRideOccurence :: Kernel.Prelude.Maybe Data.Time.UTCTime,
    pauseEndDate :: Kernel.Prelude.Maybe Data.Time.UTCTime,
    pauseStartDate :: Kernel.Prelude.Maybe Data.Time.UTCTime,
    pickupLocation :: SharedLogic.Search.SearchReqLocation,
    recurrenceEndDate :: Kernel.Prelude.Maybe Data.Time.Calendar.Day,
    recurrenceRuleDays :: [Data.Time.Calendar.DayOfWeek],
    scheduledTimeOfDay :: Data.Time.LocalTime.TimeOfDay,
    schedulingHash :: Kernel.Prelude.Maybe Data.Text.Text,
    startDatetime :: Data.Time.UTCTime,
    status :: Domain.Types.NyRegularSubscription.NyRegularSubscriptionStatus,
    updatedAt :: Data.Time.UTCTime,
    userId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    vehicleServiceTier :: Kernel.Prelude.Maybe Domain.Types.ServiceTierType.ServiceTierType
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data OneWaySearchReqDetails = OneWaySearchReqDetails
  { driverIdentifier :: Kernel.Prelude.Maybe Domain.Types.RefereeLink.DriverIdentifier,
    fareParametersInRateCard :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    isDestinationManuallyMoved :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    isMeterRideSearch :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    isReallocationEnabled :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    isSourceManuallyMoved :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    isSpecialLocation :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    placeNameSource :: Kernel.Prelude.Maybe Data.Text.Text,
    platformType :: Kernel.Prelude.Maybe Domain.Types.IntegratedBPPConfig.PlatformType,
    quotesUnifiedFlow :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    recentLocationId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.RecentLocation.RecentLocation),
    sessionToken :: Kernel.Prelude.Maybe Data.Text.Text,
    startTime :: Kernel.Prelude.Maybe Data.Time.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data UpdateSubscriptionReq = UpdateSubscriptionReq
  { id :: Kernel.Types.Id.Id Domain.Types.NyRegularSubscription.NyRegularSubscription,
    metadata :: Kernel.Prelude.Maybe Data.Aeson.Value,
    pauseEndDate :: Kernel.Prelude.Maybe Data.Time.UTCTime,
    pauseStartDate :: Kernel.Prelude.Maybe Data.Time.UTCTime,
    recurrenceEndDate :: Kernel.Prelude.Maybe Data.Time.Calendar.Day,
    recurrenceRuleDays :: Kernel.Prelude.Maybe [Data.Time.Calendar.DayOfWeek],
    scheduledTimeOfDay :: Kernel.Prelude.Maybe Data.Time.LocalTime.TimeOfDay,
    startDatetime :: Kernel.Prelude.Maybe Data.Time.UTCTime,
    status :: Kernel.Prelude.Maybe Domain.Types.NyRegularSubscription.NyRegularSubscriptionStatus
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
