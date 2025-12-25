{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.NyRegularSubscription where

import qualified Data.Aeson
import qualified Data.Time
import qualified Data.Time.Calendar
import qualified Data.Time.LocalTime
import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.Extra.NyRegularSubscription
import qualified Domain.Types.ServiceTierType
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data NyRegularSubscriptionT f = NyRegularSubscriptionT
  { bppId :: B.C f Kernel.Prelude.Text,
    createdAt :: B.C f Data.Time.UTCTime,
    dropoffLocationId :: B.C f Kernel.Prelude.Text,
    fixedPrice :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    fixedPriceCurrency :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Currency),
    fixedPriceBreakupDetails :: B.C f (Kernel.Prelude.Maybe Data.Aeson.Value),
    fixedPriceExpiryDate :: B.C f (Kernel.Prelude.Maybe Data.Time.UTCTime),
    id :: B.C f Kernel.Prelude.Text,
    initialBppQuoteId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    lastProcessedAt :: B.C f (Kernel.Prelude.Maybe Data.Time.UTCTime),
    metadata :: B.C f (Kernel.Prelude.Maybe Data.Aeson.Value),
    pauseEndDate :: B.C f (Kernel.Prelude.Maybe Data.Time.UTCTime),
    pauseStartDate :: B.C f (Kernel.Prelude.Maybe Data.Time.UTCTime),
    pickupLocationId :: B.C f Kernel.Prelude.Text,
    recurrenceEndDate :: B.C f (Kernel.Prelude.Maybe Data.Time.Calendar.Day),
    recurrenceRuleDays :: B.C f [Kernel.Prelude.Text],
    scheduledTimeOfDay :: B.C f Data.Time.LocalTime.TimeOfDay,
    schedulingHash :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    startDatetime :: B.C f Data.Time.UTCTime,
    status :: B.C f Domain.Types.Extra.NyRegularSubscription.NyRegularSubscriptionStatus,
    updatedAt :: B.C f Data.Time.UTCTime,
    userId :: B.C f Kernel.Prelude.Text,
    vehicleServiceTier :: B.C f (Kernel.Prelude.Maybe Domain.Types.ServiceTierType.ServiceTierType),
    merchantId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)
  }
  deriving (Generic, B.Beamable)

instance B.Table NyRegularSubscriptionT where
  data PrimaryKey NyRegularSubscriptionT f = NyRegularSubscriptionId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = NyRegularSubscriptionId . id

type NyRegularSubscription = NyRegularSubscriptionT Identity

$(enableKVPG ''NyRegularSubscriptionT ['id] [['userId]])

$(mkTableInstances ''NyRegularSubscriptionT "ny_regular_subscriptions")
