{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.OnboardingDocumentConfig where

import Domain.Types.Merchant (Merchant)
import EulerHS.Prelude hiding (id)
import Kernel.Types.Id

data VehicleClassCheckType = Infix | Prefix | Suffix deriving (Generic, ToJSON, FromJSON, Read, Show)

data DocumentType = RC | DL | RCInsurance deriving (Generic, ToJSON, FromJSON, Read, Eq, Ord, Show)

-- ProviderConfig?
data OnboardingDocumentConfig = OnboardingDocumentConfig
  { merchantId :: Id Merchant,
    documentType :: DocumentType,
    checkExtraction :: Bool,
    checkExpiry :: Bool,
    validVehicleClasses :: [Text],
    vehicleClassCheckType :: VehicleClassCheckType
  }
  deriving (Generic, ToJSON, FromJSON, Show)
