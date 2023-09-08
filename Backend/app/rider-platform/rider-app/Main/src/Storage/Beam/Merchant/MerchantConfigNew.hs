{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Storage.Beam.Merchant.MerchantConfigNew where

import Data.Serialize
import qualified Data.Time as Time
import qualified Database.Beam as B
import Database.Beam.MySQL ()
import qualified Domain.Types.Merchant.MerchantConfigNew as Domain
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Beckn.Context as Context
import Kernel.Types.Geofencing (GeoRestriction)
import Kernel.Utils.Common (Seconds)
import Lib.Utils ()
import Sequelize

data MerchantConfigNewT f = MerchantConfigNewT
  { merchantId :: B.C f Text,
    name :: B.C f Text,
    originRestriction :: B.C f GeoRestriction,
    destinationRestriction :: B.C f GeoRestriction,
    registryUrl :: B.C f Text,
    gatewayUrl :: B.C f Text,
    driverOfferBaseUrl :: B.C f Text,
    driverOfferApiKey :: B.C f Text,
    driverOfferMerchantId :: B.C f Text,
    city :: B.C f Context.City,
    geoHashPrecisionValue :: B.C f Int,
    country :: B.C f Context.Country,
    dirCacheSlot :: B.C f [Domain.Slot],
    timeDiffFromUtc :: B.C f Seconds,
    distanceWeightage :: B.C f Int,
    minimumDriverRatesCount :: B.C f Int,
    updatedAt :: B.C f Time.UTCTime,
    createdAt :: B.C f Time.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table MerchantConfigNewT where
  data PrimaryKey MerchantConfigNewT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . merchantId

type MerchantConfigNew = MerchantConfigNewT Identity

merchantConfigNewTMod :: MerchantConfigNewT (B.FieldModification (B.TableField MerchantConfigNewT))
merchantConfigNewTMod =
  B.tableModification
    { merchantId = B.fieldNamed "merchant_id",
      name = B.fieldNamed "name",
      originRestriction = B.fieldNamed "origin_restriction",
      destinationRestriction = B.fieldNamed "destination_restriction",
      registryUrl = B.fieldNamed "registry_url",
      gatewayUrl = B.fieldNamed "gateway_url",
      driverOfferBaseUrl = B.fieldNamed "driver_offer_base_url",
      driverOfferApiKey = B.fieldNamed "driver_offer_api_key",
      driverOfferMerchantId = B.fieldNamed "driver_offer_merchant_id",
      city = B.fieldNamed "city",
      geoHashPrecisionValue = B.fieldNamed "geo_hash_precision_value",
      country = B.fieldNamed "country",
      dirCacheSlot = B.fieldNamed "dir_cache_slot",
      timeDiffFromUtc = B.fieldNamed "time_diff_from_utc",
      distanceWeightage = B.fieldNamed "distance_weightage",
      minimumDriverRatesCount = B.fieldNamed "minimum_driver_rates_count",
      updatedAt = B.fieldNamed "updated_at",
      createdAt = B.fieldNamed "created_at"
    }

$(enableKVPG ''MerchantConfigNewT ['merchantId] [])

$(mkTableInstances ''MerchantConfigNewT "merchant_config_new" "atlas_app")
