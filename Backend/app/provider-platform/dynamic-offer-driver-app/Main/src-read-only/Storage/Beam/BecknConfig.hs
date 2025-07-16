{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.BecknConfig where

import qualified BecknV2.OnDemand.Enums
import qualified Database.Beam as B
import qualified Domain.Types.BecknConfig
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data BecknConfigT f = BecknConfigT
  { buyerFinderFee :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    collectedBy :: B.C f Domain.Types.BecknConfig.PaymentCollectedBy,
    domain :: B.C f Kernel.Prelude.Text,
    gatewayUrl :: B.C f Kernel.Prelude.Text,
    id :: B.C f Kernel.Prelude.Text,
    multimodalOnSearchTTLSec :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    onCancelTTLSec :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    onConfirmTTLSec :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    onInitTTLSec :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    onSearchTTLSec :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    onSelectTTLSec :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    onStatusTTLSec :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    onTrackTTLSec :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    onUpdateTTLSec :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    paymentParamsJson :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    registryUrl :: B.C f Kernel.Prelude.Text,
    settlementType :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    settlementWindow :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    staticTermsUrl :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    subscriberId :: B.C f Kernel.Prelude.Text,
    subscriberUrl :: B.C f Kernel.Prelude.Text,
    uniqueKeyId :: B.C f Kernel.Prelude.Text,
    vehicleCategory :: B.C f BecknV2.OnDemand.Enums.VehicleCategory,
    merchantId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table BecknConfigT where
  data PrimaryKey BecknConfigT f = BecknConfigId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = BecknConfigId . id

type BecknConfig = BecknConfigT Identity

$(enableKVPG ''BecknConfigT ['id] [])

$(mkTableInstances ''BecknConfigT "beckn_config")
