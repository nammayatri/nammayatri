{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.SearchRequest where

import qualified Database.Beam as B
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Common
import qualified Lib.Types.SpecialLocation
import Tools.Beam.UtilsTH
import qualified Tools.Maps

data SearchRequestT f = SearchRequestT
  { area :: B.C f (Kernel.Prelude.Maybe Lib.Types.SpecialLocation.Area),
    autoAssignEnabled :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    bapCity :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Beckn.Context.City),
    bapCountry :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Beckn.Context.Country),
    bapId :: B.C f Kernel.Prelude.Text,
    bapUri :: B.C f Kernel.Prelude.Text,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    currency :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Currency),
    customerCancellationDues :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    customerLanguage :: B.C f (Kernel.Prelude.Maybe Tools.Maps.Language),
    device :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    disabilityTag :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    driverDefaultExtraFee :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Money),
    driverDefaultExtraFeeAmount :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    estimatedDistance :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Meters),
    estimatedDuration :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Seconds),
    fromLocationId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    id :: B.C f Kernel.Prelude.Text,
    isBlockedRoute :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    isCustomerPrefferedSearchRoute :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    isReallocationEnabled :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    isScheduled :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    messageId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    pickupZoneGateId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    providerId :: B.C f Kernel.Prelude.Text,
    riderId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    specialLocationTag :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    startTime :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    toLocationId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    tollCharges :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    tollNames :: B.C f (Kernel.Prelude.Maybe [Kernel.Prelude.Text]),
    transactionId :: B.C f Kernel.Prelude.Text,
    validTill :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table SearchRequestT where
  data PrimaryKey SearchRequestT f = SearchRequestId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = SearchRequestId . id

type SearchRequest = SearchRequestT Identity

$(enableKVPG ''SearchRequestT ['id] [['transactionId]])

$(mkTableInstances ''SearchRequestT "search_request")
