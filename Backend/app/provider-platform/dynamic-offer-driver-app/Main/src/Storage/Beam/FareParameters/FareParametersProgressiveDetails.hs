{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Beam.FareParameters.FareParametersProgressiveDetails where

import qualified Data.Aeson as A
import qualified Data.HashMap.Internal as HM
import qualified Data.Map.Strict as M
import Data.Serialize
import qualified Database.Beam as B
import Database.Beam.Backend ()
import Database.Beam.MySQL ()
-- import qualified Domain.Types.FarePolicy.FareParametersProgressiveDetails as Domain

import qualified Domain.Types.Vehicle.Variant as Vehicle
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common hiding (id)
import Lib.Utils ()
import Lib.UtilsTH
import Sequelize as Se
import Storage.Tabular.Vehicle ()

-- fromFieldEnum ::
--   (Typeable a, Read a) =>
--   DPSF.Field ->
--   Maybe ByteString ->
--   DPSF.Conversion a
-- fromFieldEnum f mbValue = case mbValue of
--   Nothing -> DPSF.returnError UnexpectedNull f mempty
--   Just value' ->
--     case (readMaybe (unpackChars value')) of
--       Just val -> pure val
--       _ -> DPSF.returnError ConversionFailed f "Could not 'read' value for 'Rule'."

-- instance FromField Vehicle.Variant where
--   fromField = fromFieldEnum

-- instance HasSqlValueSyntax be String => HasSqlValueSyntax be Vehicle.Variant where
--   sqlValueSyntax = autoSqlValueSyntax

-- instance BeamSqlBackend be => B.HasSqlEqualityCheck be Vehicle.Variant

-- instance FromBackendRow Postgres Vehicle.Variant

instance IsString Vehicle.Variant where
  fromString = show

-- instance FromField Meters where
--   fromField = fromFieldEnum

-- instance HasSqlValueSyntax be String => HasSqlValueSyntax be Meters where
--   sqlValueSyntax = autoSqlValueSyntax

-- instance BeamSqlBackend be => B.HasSqlEqualityCheck be Meters

-- instance FromBackendRow Postgres Meters

instance IsString Meters where
  fromString = show

-- instance FromField Money where
--   fromField = fromFieldEnum

-- instance HasSqlValueSyntax be String => HasSqlValueSyntax be Money where
--   sqlValueSyntax = autoSqlValueSyntax

-- instance BeamSqlBackend be => B.HasSqlEqualityCheck be Money

-- instance FromBackendRow Postgres Money

-- deriving stock instance Read Money

instance IsString Money where
  fromString = show

data FareParametersProgressiveDetailsT f = FareParametersProgressiveDetailsT
  { fareParametersId :: B.C f Text,
    deadKmFare :: B.C f Money,
    extraKmFare :: B.C f (Maybe Money)
  }
  deriving (Generic, B.Beamable)

instance B.Table FareParametersProgressiveDetailsT where
  data PrimaryKey FareParametersProgressiveDetailsT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . fareParametersId

instance ModelMeta FareParametersProgressiveDetailsT where
  modelFieldModification = fareParametersProgressiveDetailsTMod
  modelTableName = "fare_parameters_progressive_details"
  mkExprWithDefault _ = B.insertExpressions []
  modelSchemaName = Just "atlas_driver_offer_bpp"

type FareParametersProgressiveDetails = FareParametersProgressiveDetailsT Identity

instance FromJSON FareParametersProgressiveDetails where
  parseJSON = A.genericParseJSON A.defaultOptions

instance ToJSON FareParametersProgressiveDetails where
  toJSON = A.genericToJSON A.defaultOptions

deriving stock instance Show FareParametersProgressiveDetails

fareParametersProgressiveDetailsTMod :: FareParametersProgressiveDetailsT (B.FieldModification (B.TableField FareParametersProgressiveDetailsT))
fareParametersProgressiveDetailsTMod =
  B.tableModification
    { fareParametersId = B.fieldNamed "fare_parameters_id",
      deadKmFare = B.fieldNamed "dead_km_fare",
      extraKmFare = B.fieldNamed "extra_km_fare"
    }

instance Serialize FareParametersProgressiveDetails where
  put = error "undefined"
  get = error "undefined"

psToHs :: HM.HashMap Text Text
psToHs = HM.empty

fareParametersProgressiveDetailsToHSModifiers :: M.Map Text (A.Value -> A.Value)
fareParametersProgressiveDetailsToHSModifiers =
  M.empty

fareParametersProgressiveDetailsToPSModifiers :: M.Map Text (A.Value -> A.Value)
fareParametersProgressiveDetailsToPSModifiers =
  M.empty

-- transformBeamFareParametersProgressiveDetailsToDomain :: FareParametersProgressiveDetails -> DomainFPPD.FullFareParametersProgressiveDetails
-- transformBeamFareParametersProgressiveDetailsToDomain FareParametersProgressiveDetailsT {..} = do
--   ( (KTI.Id fareParametersId),
--     Domain.FParamsProgressiveDetails
--       { deadKmFare = deadKmFare,
--         extraKmFare = extraKmFare
--       }
--     )

-- transformDomainFareParametersProgressiveDetailsToBeam :: DomainFPPD.FullFareParametersProgressiveDetails -> FareParametersProgressiveDetails
-- transformDomainFareParametersProgressiveDetailsToBeam (KTI.Id fareParametersId, Domain.FParamsProgressiveDetails {..}) =
--   FareParametersProgressiveDetailsT
--     { fareParametersId = fareParametersId,
--       deadKmFare = deadKmFare,
--       extraKmFare = extraKmFare
--     }

-- findById' :: L.MonadFlow m => KTI.Id Domain.FareParameters -> m (Maybe DomainFPPD.FullFareParametersProgressiveDetails)
-- findById' (KTI.Id fareParametersId') = do
--   dbConf <- L.getOption KBT.PsqlDbCfg
--   case dbConf of
--     Just dbCOnf' -> either (pure Nothing) (transformBeamFareParametersProgressiveDetailsToDomain <$>) <$> KV.findWithKVConnector dbCOnf' Mesh.meshConfig [Se.Is fareParametersId $ Se.Eq fareParametersId']
--     Nothing -> pure Nothing

$(enableKVPG ''FareParametersProgressiveDetailsT ['fareParametersId] [])
