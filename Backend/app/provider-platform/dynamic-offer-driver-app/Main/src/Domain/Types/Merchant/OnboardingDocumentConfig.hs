{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.Merchant.OnboardingDocumentConfig where

import Domain.Types.Merchant (Merchant)
import Domain.Types.Vehicle (Variant)
import Kernel.Prelude
import Kernel.Types.Id
import Tools.Beam.UtilsTH (mkBeamInstancesForEnum)

data VehicleClassCheckType = Infix | Prefix | Suffix deriving (Generic, ToJSON, FromJSON, Read, Show, Ord, Eq)

$(mkBeamInstancesForEnum ''VehicleClassCheckType)

data DocumentType = RC | DL | RCInsurance deriving (Generic, ToJSON, FromJSON, Read, Eq, Ord, Show)

$(mkBeamInstancesForEnum ''DocumentType)

data SupportedVehicleClasses = DLValidClasses [Text] | RCValidClasses [VehicleClassVariantMap] deriving (Generic, ToJSON, FromJSON, Show)

data VehicleClassVariantMap = VehicleClassVariantMap
  { vehicleClass :: Text,
    vehicleCapacity :: Maybe Int,
    vehicleVariant :: Variant
  }
  deriving (Generic, ToJSON, FromJSON, Show)

data OnboardingDocumentConfig = OnboardingDocumentConfig
  { merchantId :: Id Merchant,
    documentType :: DocumentType,
    checkExtraction :: Bool,
    checkExpiry :: Bool,
    supportedVehicleClasses :: SupportedVehicleClasses,
    vehicleClassCheckType :: VehicleClassCheckType,
    rcNumberPrefix :: Text,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, ToJSON, FromJSON, Show)
