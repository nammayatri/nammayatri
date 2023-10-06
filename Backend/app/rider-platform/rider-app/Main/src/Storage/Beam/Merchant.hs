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
{-# LANGUAGE TemplateHaskell #-}

module Storage.Beam.Merchant where

import Data.ByteString.Internal (ByteString)
import qualified Data.Vector as V
import qualified Database.Beam as B
import Database.PostgreSQL.Simple.FromField (fromField)
import qualified Database.PostgreSQL.Simple.FromField as DPSF
import qualified Domain.Types.Merchant as Domain
import Kernel.Prelude
import Kernel.Types.Base64
import Kernel.Types.Beckn.Context as Context
import Kernel.Types.Geofencing (GeoRestriction)
import qualified Kernel.Types.Geofencing as Geo
import Kernel.Utils.Common (Seconds)
import qualified Tools.Beam.UtilsTH as TH

fromFieldEnum' ::
  DPSF.Field ->
  Maybe ByteString ->
  DPSF.Conversion GeoRestriction
fromFieldEnum' f mbValue = case mbValue of
  Nothing -> pure Geo.Unrestricted
  Just _ -> Geo.Regions . V.toList <$> fromField f mbValue

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
    minimumDriverRatesCount :: B.C f Int,
    cipherText :: B.C f (Maybe Base64),
    signatureExpiry :: B.C f Int,
    distanceWeightage :: B.C f Int,
    updatedAt :: B.C f UTCTime,
    createdAt :: B.C f UTCTime,
    timeDiffFromUtc :: B.C f Seconds,
    dirCacheSlot :: B.C f [Domain.Slot],
    isAvoidToll :: B.C f Bool,
    aadhaarVerificationTryLimit :: B.C f Int,
    aadhaarKeyExpiryTime :: B.C f Seconds
  }
  deriving (Generic, B.Beamable)

instance B.Table MerchantT where
  data PrimaryKey MerchantT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type Merchant = MerchantT Identity

$(TH.enableKVPG ''MerchantT ['id] [['shortId], ['subscriberId]])

$(TH.mkTableInstances ''MerchantT "merchant")
