{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Beam.Merchant where

import qualified Data.Aeson as A
import Data.ByteString.Internal (ByteString)
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Serialize
import qualified Data.Text.Encoding as TE
import qualified Data.Time as Time
import qualified Data.Vector as V
import qualified Database.Beam as B
import Database.Beam.Backend
import Database.Beam.MySQL ()
import Database.Beam.Postgres
  ( Postgres,
  )
import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import qualified Database.PostgreSQL.Simple.FromField as DPSF
import Debug.Trace as T
import qualified Domain.Types.Merchant as Domain
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Base64
import Kernel.Types.Beckn.Context as Context
import Kernel.Types.Geofencing (GeoRestriction)
import qualified Kernel.Types.Geofencing as Geo
import Kernel.Utils.Common (Seconds)
import Lib.Utils ()
import Sequelize

fromFieldEnum' ::
  DPSF.Field ->
  Maybe ByteString ->
  DPSF.Conversion GeoRestriction
fromFieldEnum' f mbValue = case mbValue of
  Nothing -> pure Geo.Unrestricted
  Just _ -> Geo.Regions . V.toList <$> fromField f mbValue

deriving newtype instance FromField Base64

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Base64 where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Base64

instance FromBackendRow Postgres Base64

instance FromField GeoRestriction where
  fromField = fromFieldEnum'

instance HasSqlValueSyntax be (Maybe [Text]) => HasSqlValueSyntax be GeoRestriction where
  sqlValueSyntax = sqlValueSyntax . Geo.geoRestrictionToMaybeList

instance BeamSqlBackend be => B.HasSqlEqualityCheck be GeoRestriction

instance FromBackendRow Postgres GeoRestriction

deriving stock instance Eq GeoRestriction

deriving stock instance Ord GeoRestriction

deriving stock instance Ord Base64

deriving stock instance Read Base64

instance IsString GeoRestriction where
  fromString = show

deriving stock instance Ord Context.City

deriving stock instance Ord Context.Country

instance IsString Context.City where
  fromString = show

instance IsString Context.Country where
  fromString = show

data MerchantT f = MerchantT
  { id :: B.C f Text,
    shortId :: B.C f Text,
    subscriberId :: B.C f Text,
    name :: B.C f Text,
    city :: B.C f Context.City,
    country :: B.C f Context.Country,
    bapId :: B.C f Text,
    bapUniqueKeyId :: B.C f Text,
    originRestriction :: B.C f GeoRestriction,
    destinationRestriction :: B.C f GeoRestriction,
    gatewayUrl :: B.C f Text,
    registryUrl :: B.C f Text,
    driverOfferBaseUrl :: B.C f Text,
    driverOfferApiKey :: B.C f Text,
    driverOfferMerchantId :: B.C f Text,
    geoHashPrecisionValue :: B.C f Int,
    signingPublicKey :: B.C f Base64,
    cipherText :: B.C f (Maybe Base64),
    signatureExpiry :: B.C f Int,
    distanceWeightage :: B.C f Int,
    updatedAt :: B.C f Time.UTCTime,
    createdAt :: B.C f Time.UTCTime,
    timeDiffFromUtc :: B.C f Seconds,
    dirCacheSlot :: B.C f [Domain.Slot],
    isAvoidToll :: B.C f Bool
  }
  deriving (Generic, B.Beamable)

instance B.Table MerchantT where
  data PrimaryKey MerchantT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type Merchant = MerchantT Identity

fromFieldJSON' ::
  DPSF.Field ->
  Maybe ByteString ->
  DPSF.Conversion [Domain.Slot]
fromFieldJSON' f mbValue = case mbValue of
  Nothing -> DPSF.returnError DPSF.UnexpectedNull f mempty
  Just value' -> case (A.decode $ fromStrict value' :: Maybe (V.Vector Domain.Slot)) of
    Just res -> pure $ V.toList res
    Nothing -> DPSF.returnError DPSF.ConversionFailed f ("Could not 'read'" <> show value')

fromFieldSlots ::
  DPSF.Field ->
  Maybe ByteString ->
  DPSF.Conversion [Domain.Slot]
fromFieldSlots f mbValue = do
  value <- fromField f mbValue
  case (A.fromJSON value :: A.Result (V.Vector Domain.Slot)) of
    A.Success a -> pure $ V.toList a
    _ -> DPSF.returnError DPSF.ConversionFailed f "Conversion failed for"

instance HasSqlValueSyntax be A.Value => HasSqlValueSyntax be [Domain.Slot] where
  sqlValueSyntax = sqlValueSyntax . (A.String . TE.decodeUtf8 . toStrict . A.encode . A.toJSON)

instance BeamSqlBackend be => B.HasSqlEqualityCheck be [Domain.Slot]

instance FromBackendRow Postgres [Domain.Slot] where
  fromBackendRow = do
    textVal <- fromBackendRow
    case T.trace (show textVal) $ A.fromJSON textVal of
      A.Success (jsonVal :: Text) -> case A.eitherDecode (fromStrict $ TE.encodeUtf8 jsonVal) of
        Right val -> pure val
        Left err -> fail ("Error Can't Decode Array of Domain slot :: Error :: " <> err)
      A.Error err -> fail ("Error Can't Decode Array of Domain slot :: Error :: " <> err)

deriving stock instance Ord Domain.Slot

merchantTMod :: MerchantT (B.FieldModification (B.TableField MerchantT))
merchantTMod =
  B.tableModification
    { id = B.fieldNamed "id",
      shortId = B.fieldNamed "short_id",
      subscriberId = B.fieldNamed "subscriber_id",
      name = B.fieldNamed "name",
      city = B.fieldNamed "city",
      country = B.fieldNamed "country",
      bapId = B.fieldNamed "bap_id",
      bapUniqueKeyId = B.fieldNamed "bap_unique_key_id",
      originRestriction = B.fieldNamed "origin_restriction",
      destinationRestriction = B.fieldNamed "destination_restriction",
      gatewayUrl = B.fieldNamed "gateway_url",
      registryUrl = B.fieldNamed "registry_url",
      driverOfferBaseUrl = B.fieldNamed "driver_offer_base_url",
      driverOfferApiKey = B.fieldNamed "driver_offer_api_key",
      driverOfferMerchantId = B.fieldNamed "driver_offer_merchant_id",
      geoHashPrecisionValue = B.fieldNamed "geo_hash_precision_value",
      signingPublicKey = B.fieldNamed "signing_public_key",
      cipherText = B.fieldNamed "cipher_text",
      signatureExpiry = B.fieldNamed "signature_expiry",
      distanceWeightage = B.fieldNamed "distance_weightage",
      updatedAt = B.fieldNamed "updated_at",
      createdAt = B.fieldNamed "created_at",
      timeDiffFromUtc = B.fieldNamed "time_diff_from_utc",
      dirCacheSlot = B.fieldNamed "dir_cache_slot"
    }

$(enableKVPG ''MerchantT ['id] [['shortId], ['subscriberId]])

$(mkTableInstances ''MerchantT "merchant" "atlas_app")
