{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.SharedSearchRequest where

import qualified BecknV2.OnDemand.Enums
import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.Common
import qualified Domain.Types.SharedSearchRequest
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data SharedSearchRequestT f = SharedSearchRequestT
  { createdAt :: (B.C f Kernel.Prelude.UTCTime),
    estimatedDistance :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Meters)),
    estimatedDuration :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Seconds)),
    fromLocationIds :: (B.C f [Kernel.Prelude.Text]),
    id :: (B.C f Kernel.Prelude.Text),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    status :: (B.C f Domain.Types.SharedSearchRequest.SearchRequestStatus),
    toLocationIds :: (B.C f [Kernel.Prelude.Text]),
    tollCharges :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
    tollNames :: (B.C f (Kernel.Prelude.Maybe [Kernel.Prelude.Text])),
    transactionId :: (B.C f Kernel.Prelude.Text),
    tripCategory :: (B.C f (Kernel.Prelude.Maybe Domain.Types.Common.TripCategory)),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime),
    validTill :: (B.C f Kernel.Prelude.UTCTime),
    vehicleCategory :: (B.C f (Kernel.Prelude.Maybe BecknV2.OnDemand.Enums.VehicleCategory))
  }
  deriving (Generic, B.Beamable)

instance B.Table SharedSearchRequestT where
  data PrimaryKey SharedSearchRequestT f = SharedSearchRequestId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = SharedSearchRequestId . id

type SharedSearchRequest = SharedSearchRequestT Identity

$(enableKVPG (''SharedSearchRequestT) [('id)] [])

$(mkTableInstances (''SharedSearchRequestT) "shared_search_request")
