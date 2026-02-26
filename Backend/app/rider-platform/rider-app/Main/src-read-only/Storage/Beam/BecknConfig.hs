{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.BecknConfig where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Prelude
import qualified BecknV2.FRFS.Enums
import qualified BecknV2.OnDemand.Enums
import qualified Database.Beam as B



data BecknConfigT f
    = BecknConfigT {bapIFSC :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                    buyerFinderFee :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                    cancelTTLSec :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
                    collectedBy :: (B.C f BecknV2.FRFS.Enums.Network),
                    confirmBufferTTLSec :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
                    confirmTTLSec :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
                    domain :: (B.C f Kernel.Prelude.Text),
                    gatewayUrl :: (B.C f Kernel.Prelude.Text),
                    id :: (B.C f Kernel.Prelude.Text),
                    initTTLSec :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
                    paymentParamsJson :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                    ppfEnabled :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool)),
                    ratingTTLSec :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
                    registryUrl :: (B.C f Kernel.Prelude.Text),
                    searchTTLSec :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
                    selectTTLSec :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
                    settlementType :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                    settlementWindow :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                    staticTermsUrl :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                    statusTTLSec :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
                    subscriberId :: (B.C f Kernel.Prelude.Text),
                    subscriberUrl :: (B.C f Kernel.Prelude.Text),
                    trackTTLSec :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
                    uniqueKeyId :: (B.C f Kernel.Prelude.Text),
                    vehicleCategory :: (B.C f BecknV2.OnDemand.Enums.VehicleCategory),
                    merchantId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
                    merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
                    createdAt :: (B.C f Kernel.Prelude.UTCTime),
                    updatedAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table BecknConfigT
    where data PrimaryKey BecknConfigT f = BecknConfigId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = BecknConfigId . id
type BecknConfig = BecknConfigT Identity

$(enableKVPG (''BecknConfigT) [('id)] [])

$(mkTableInstances (''BecknConfigT) "beckn_config")

