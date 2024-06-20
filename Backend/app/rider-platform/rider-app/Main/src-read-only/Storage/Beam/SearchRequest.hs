{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.SearchRequest where

import qualified Database.Beam as B
import qualified Domain.Types.SearchRequest
import Kernel.External.Encryption
import qualified Kernel.External.Maps
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Version
import qualified Kernel.Utils.Common
import Tools.Beam.UtilsTH

data SearchRequestT f = SearchRequestT
  { id :: B.C f Kernel.Prelude.Text,
    startTime :: B.C f Kernel.Prelude.UTCTime,
    returnTime :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    roundTrip :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    validTill :: B.C f Kernel.Prelude.UTCTime,
    riderId :: B.C f Kernel.Prelude.Text,
    clientId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    fromLocationId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    toLocationId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    distance :: B.C f (Kernel.Prelude.Maybe Kernel.Utils.Common.Centesimal),
    distanceValue :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecDistance),
    maxDistance :: B.C f (Kernel.Prelude.Maybe Kernel.Utils.Common.Centesimal),
    maxDistanceValue :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecDistance),
    distanceUnit :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.DistanceUnit),
    estimatedRideDuration :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Seconds),
    device :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    language :: B.C f (Kernel.Prelude.Maybe Kernel.External.Maps.Language),
    disabilityTag :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    currency :: B.C f (Kernel.Prelude.Maybe Kernel.Utils.Common.Currency),
    customerExtraFee :: B.C f (Kernel.Prelude.Maybe Kernel.Utils.Common.Money),
    customerExtraFeeAmount :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    autoAssignEnabled :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    autoAssignEnabledV2 :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    availablePaymentMethods :: B.C f [Kernel.Prelude.Text],
    selectedPaymentMethodId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    riderPreferredOption :: B.C f (Kernel.Prelude.Maybe Domain.Types.SearchRequest.RiderPreferredOption),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    clientBundleVersion :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    clientSdkVersion :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    clientConfigVersion :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    clientOsType :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Version.DeviceType),
    clientOsVersion :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    backendConfigVersion :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    backendAppVersion :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    isAdvanceBookingEnabled :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool)
  }
  deriving (Generic, B.Beamable)

instance B.Table SearchRequestT where
  data PrimaryKey SearchRequestT f = SearchRequestId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = SearchRequestId . id

type SearchRequest = SearchRequestT Identity

$(enableKVPG ''SearchRequestT ['id] [['riderId]])

$(mkTableInstances ''SearchRequestT "search_request")
