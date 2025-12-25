{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.NyRegularSubscription (module Domain.Types.NyRegularSubscription, module ReExport) where

import Data.Aeson
import qualified Data.Time
import qualified Data.Time.Calendar
import qualified Data.Time.LocalTime
import Domain.Types.Extra.NyRegularSubscription as ReExport
import qualified Domain.Types.Extra.NyRegularSubscription
import qualified Domain.Types.Location
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Domain.Types.ServiceTierType
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data NyRegularSubscription = NyRegularSubscription
  { bppId :: Kernel.Prelude.Text,
    createdAt :: Data.Time.UTCTime,
    dropoffLocation :: Domain.Types.Location.Location,
    fixedPrice :: Kernel.Prelude.Maybe Kernel.Types.Common.Price,
    fixedPriceBreakupDetails :: Kernel.Prelude.Maybe Data.Aeson.Value,
    fixedPriceExpiryDate :: Kernel.Prelude.Maybe Data.Time.UTCTime,
    id :: Kernel.Types.Id.Id Domain.Types.NyRegularSubscription.NyRegularSubscription,
    initialBppQuoteId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    lastProcessedAt :: Kernel.Prelude.Maybe Data.Time.UTCTime,
    metadata :: Kernel.Prelude.Maybe Data.Aeson.Value,
    pauseEndDate :: Kernel.Prelude.Maybe Data.Time.UTCTime,
    pauseStartDate :: Kernel.Prelude.Maybe Data.Time.UTCTime,
    pickupLocation :: Domain.Types.Location.Location,
    recurrenceEndDate :: Kernel.Prelude.Maybe Data.Time.Calendar.Day,
    recurrenceRuleDays :: [Data.Time.Calendar.DayOfWeek],
    scheduledTimeOfDay :: Data.Time.LocalTime.TimeOfDay,
    schedulingHash :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    startDatetime :: Data.Time.UTCTime,
    status :: Domain.Types.Extra.NyRegularSubscription.NyRegularSubscriptionStatus,
    updatedAt :: Data.Time.UTCTime,
    userId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    vehicleServiceTier :: Kernel.Prelude.Maybe Domain.Types.ServiceTierType.ServiceTierType,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity)
  }
  deriving (Generic, Eq, Show, ToJSON, FromJSON, ToSchema)
