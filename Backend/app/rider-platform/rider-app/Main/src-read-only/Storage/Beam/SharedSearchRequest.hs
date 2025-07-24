{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.SharedSearchRequest where

import qualified BecknV2.OnDemand.Enums
import qualified Data.Aeson
import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.SharedSearchRequest
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Utils.Common
import Tools.Beam.UtilsTH

data SharedSearchRequestT f = SharedSearchRequestT
  { createdAt :: (B.C f Kernel.Prelude.UTCTime),
    id :: (B.C f Kernel.Prelude.Text),
    distanceUnit :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.DistanceUnit)),
    maxDistance :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Centesimal)),
    maxDistanceValue :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecDistance)),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    searchRequestIds :: (B.C f [Kernel.Prelude.Text]),
    status :: (B.C f Domain.Types.SharedSearchRequest.SharedSearchRequestStatus),
    currency :: (B.C f (Kernel.Prelude.Maybe Kernel.Utils.Common.Currency)),
    totalCustomerExtraFee :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Money)),
    totalCustomerExtraFeeAmount :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime),
    validTill :: (B.C f Kernel.Prelude.UTCTime),
    vehicleCategory :: (B.C f BecknV2.OnDemand.Enums.VehicleCategory),
    waypoints :: (B.C f Data.Aeson.Value)
  }
  deriving (Generic, B.Beamable)

instance B.Table SharedSearchRequestT where
  data PrimaryKey SharedSearchRequestT f = SharedSearchRequestId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = SharedSearchRequestId . id

type SharedSearchRequest = SharedSearchRequestT Identity

$(enableKVPG (''SharedSearchRequestT) [('id)] [])

$(mkTableInstances (''SharedSearchRequestT) "shared_search_request")
