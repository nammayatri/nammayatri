{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.Merchant where

import qualified Database.Beam as B
import qualified Domain.Types
import Domain.Types.Common ()
import qualified Domain.Types.Merchant
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Geofencing
import Tools.Beam.UtilsTH

data MerchantT f = MerchantT
  { city :: (B.C f Kernel.Types.Beckn.Context.City),
    country :: (B.C f Kernel.Types.Beckn.Context.Country),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    description :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    enabled :: (B.C f Kernel.Prelude.Bool),
    fleetOwnerEnabledCheck :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    fromTime :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
    gatewayAndRegistryPriorityList :: (B.C f (Kernel.Prelude.Maybe [Domain.Types.GatewayAndRegistryService])),
    geoHashPrecisionValue :: (B.C f Kernel.Prelude.Int),
    destinationRestriction :: (B.C f Kernel.Types.Geofencing.GeoRestriction),
    originRestriction :: (B.C f Kernel.Types.Geofencing.GeoRestriction),
    gstin :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    headCount :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
    id :: (B.C f Kernel.Prelude.Text),
    info :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    internalApiKey :: (B.C f Kernel.Prelude.Text),
    minimumDriverRatesCount :: (B.C f Kernel.Prelude.Int),
    mobileCountryCode :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    mobileNumber :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    name :: (B.C f Kernel.Prelude.Text),
    onlinePayment :: (B.C f Kernel.Prelude.Bool),
    overwriteAssociation :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    registryUrl :: (B.C f Kernel.Prelude.Text),
    shortId :: (B.C f Kernel.Prelude.Text),
    state :: (B.C f Kernel.Types.Beckn.Context.IndianState),
    status :: (B.C f Domain.Types.Merchant.Status),
    subscriberId :: (B.C f Kernel.Prelude.Text),
    toTime :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
    uniqueKeyId :: (B.C f Kernel.Prelude.Text),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime),
    verified :: (B.C f Kernel.Prelude.Bool)
  }
  deriving (Generic, B.Beamable)

instance B.Table MerchantT where
  data PrimaryKey MerchantT f = MerchantId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = MerchantId . id

type Merchant = MerchantT Identity

$(enableKVPG (''MerchantT) [('id)] [[('shortId)], [('subscriberId)]])

$(mkTableInstances (''MerchantT) "merchant")
