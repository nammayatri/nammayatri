{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.DriverOnboarding.Image where

import Domain.Types.DriverOnboarding.Error
import Domain.Types.Merchant
import Domain.Types.Person
import Kernel.Prelude
import Kernel.Types.Id
import qualified Tools.Beam.UtilsTH as TH

data ImageType = DriverLicense | VehicleRegistrationCertificate
  deriving (Show, Eq, Read, Generic, Enum, Bounded, FromJSON, ToJSON, ToSchema, Ord)

$(TH.mkBeamInstancesForEnum ''ImageType)

data Image = Image
  { id :: Id Image,
    personId :: Id Person,
    merchantId :: Id Merchant,
    s3Path :: Text,
    imageType :: ImageType,
    isValid :: Bool,
    failureReason :: Maybe DriverOnboardingError,
    createdAt :: UTCTime
  }
  deriving (Generic, ToSchema, ToJSON, FromJSON)
