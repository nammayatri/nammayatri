{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Storage.Beam.Merchant.MerchantConfigNew where

import Data.ByteString.Internal (ByteString)
import Data.Serialize
import qualified Data.Time as Time
import qualified Data.Vector as V
import qualified Database.Beam as B
import Database.Beam.MySQL ()
import Database.PostgreSQL.Simple.FromField (fromField)
import qualified Database.PostgreSQL.Simple.FromField as DPSF
import qualified Domain.Types.Merchant.MerchantConfigNew as Domain
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Beckn.Context as Context
import Kernel.Types.Geofencing (GeoRestriction)
import qualified Kernel.Types.Geofencing as Geo
import Kernel.Utils.Common (Seconds)
import Lib.Utils ()
import Sequelize
import Tools.Beam.UtilsTH

fromFieldEnum' ::
  DPSF.Field ->
  Maybe ByteString ->
  DPSF.Conversion GeoRestriction
fromFieldEnum' f mbValue = case mbValue of
  Nothing -> pure Geo.Unrestricted
  Just _ -> Geo.Regions . V.toList <$> fromField f mbValue

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

$(enableKVPG ''MerchantConfigNewT ['merchantId] [])

$(mkTableInstances ''MerchantConfigNewT "merchant_config_new")
