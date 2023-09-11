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

module Storage.Beam.Merchant where

-- import Data.ByteString.Internal (ByteString)
import Data.Serialize
import qualified Data.Time as Time
-- import qualified Data.Vector as V
import qualified Database.Beam as B
import Database.Beam.MySQL ()
-- import Database.PostgreSQL.Simple.FromField (fromField)
-- import qualified Database.PostgreSQL.Simple.FromField as DPSF
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Base64
-- import Kernel.Types.Geofencing (GeoRestriction)
-- import qualified Kernel.Types.Geofencing as Geo
import Lib.Utils ()
import Sequelize
import Tools.Beam.UtilsTH

data MerchantT f = MerchantT
  { id :: B.C f Text,
    shortId :: B.C f Text,
    subscriberId :: B.C f Text,
    name :: B.C f Text,
    bapId :: B.C f Text,
    bapUniqueKeyId :: B.C f Text,
    gatewayUrl :: B.C f Text,
    registryUrl :: B.C f Text,
    signingPublicKey :: B.C f Base64,
    cipherText :: B.C f (Maybe Base64),
    signatureExpiry :: B.C f Int,
    updatedAt :: B.C f Time.UTCTime,
    createdAt :: B.C f Time.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table MerchantT where
  data PrimaryKey MerchantT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type Merchant = MerchantT Identity

$(enableKVPG ''MerchantT ['id] [['shortId], ['subscriberId]])

$(mkTableInstances ''MerchantT "merchant")
