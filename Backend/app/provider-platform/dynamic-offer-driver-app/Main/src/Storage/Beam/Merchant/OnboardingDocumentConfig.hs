{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Storage.Beam.Merchant.OnboardingDocumentConfig where

import qualified Data.Aeson as A
import Data.Serialize
import qualified Data.Time as Time
import qualified Database.Beam as B
import Database.Beam.MySQL ()
import qualified Domain.Types.Merchant.OnboardingDocumentConfig as Domain
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
-- import Kernel.Types.Common hiding (id)

import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude hiding (Generic)
import Kernel.Utils.Common (encodeToText)
import Lib.Utils ()
import Sequelize

data OnboardingDocumentConfigT f = OnboardingDocumentConfigT
  { merchantId :: B.C f Text,
    documentType :: B.C f Domain.DocumentType,
    checkExtraction :: B.C f Bool,
    checkExpiry :: B.C f Bool,
    supportedVehicleClassesJSON :: B.C f A.Value,
    rcNumberPrefix :: B.C f Text,
    vehicleClassCheckType :: B.C f Domain.VehicleClassCheckType,
    createdAt :: B.C f Time.UTCTime,
    updatedAt :: B.C f Time.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table OnboardingDocumentConfigT where
  data PrimaryKey OnboardingDocumentConfigT f
    = Id (B.C f Text) (B.C f Domain.DocumentType)
    deriving (Generic, B.Beamable)
  primaryKey = Id <$> merchantId <*> documentType

type OnboardingDocumentConfig = OnboardingDocumentConfigT Identity

getConfigJSON :: Domain.SupportedVehicleClasses -> Text
getConfigJSON = \case
  Domain.DLValidClasses cfg -> encodeToText cfg
  Domain.RCValidClasses cfg -> encodeToText cfg

onboardingDocumentConfigTMod :: OnboardingDocumentConfigT (B.FieldModification (B.TableField OnboardingDocumentConfigT))
onboardingDocumentConfigTMod =
  B.tableModification
    { merchantId = B.fieldNamed "merchant_id",
      documentType = B.fieldNamed "document_type",
      checkExtraction = B.fieldNamed "check_extraction",
      checkExpiry = B.fieldNamed "check_expiry",
      supportedVehicleClassesJSON = B.fieldNamed "supported_vehicle_classes_json",
      rcNumberPrefix = B.fieldNamed "rc_number_prefix",
      vehicleClassCheckType = B.fieldNamed "vehicle_class_check_type",
      createdAt = B.fieldNamed "created_at",
      updatedAt = B.fieldNamed "updated_at"
    }

$(enableKVPG ''OnboardingDocumentConfigT ['merchantId, 'documentType] [['merchantId]])

$(mkTableInstances ''OnboardingDocumentConfigT "onboarding_document_configs" "atlas_driver_offer_bpp")
