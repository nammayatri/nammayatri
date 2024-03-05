{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.BecknConfig where

import qualified Database.Beam as B
import qualified Domain.Types.BecknConfig
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Servant.Client.Core
import Tools.Beam.UtilsTH

data BecknConfigT f = BecknConfigT
  { buyerFinderFee :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    cancelTTLSec :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    collectedBy :: B.C f Domain.Types.BecknConfig.PaymentCollectedBy,
    confirmBufferTTLSec :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    confirmTTLSec :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    domain :: B.C f Kernel.Prelude.Text,
    gatewayUrl :: B.C f Kernel.Prelude.Text,
    id :: B.C f Kernel.Prelude.Text,
    initTTLSec :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    paymentParamsJson :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    ratingTTLSec :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    registryUrl :: B.C f Kernel.Prelude.Text,
    searchTTLSec :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    selectTTLSec :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    settlementType :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    settlementWindow :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    staticTermsUrl :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    statusTTLSec :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    subscriberId :: B.C f Kernel.Prelude.Text,
    subscriberUrl :: B.C f Kernel.Prelude.Text,
    trackTTLSec :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    uniqueKeyId :: B.C f Kernel.Prelude.Text,
    vehicleCategory :: B.C f Domain.Types.BecknConfig.VehicleCategory,
    merchantId :: B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text)),
    merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text)),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table BecknConfigT where
  data PrimaryKey BecknConfigT f = BecknConfigId (B.C f Kernel.Prelude.Text)
    deriving (Generic, B.Beamable)
  primaryKey = BecknConfigId . id

type BecknConfig = BecknConfigT Identity

$(enableKVPG ''BecknConfigT ['id] [])

$(mkTableInstances ''BecknConfigT "beckn_config")
