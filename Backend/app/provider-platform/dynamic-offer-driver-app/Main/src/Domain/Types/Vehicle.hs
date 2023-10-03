{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TemplateHaskell #-}

module Domain.Types.Vehicle (module Domain.Types.Vehicle, module Reexport) where

import Data.Aeson
import Data.OpenApi (ToParamSchema, ToSchema)
import Data.Time
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DPers
import Domain.Types.Vehicle.Variant as Reexport
import EulerHS.Prelude hiding (id)
import Kernel.Types.Id
import Kernel.Utils.GenericPretty
import Kernel.Utils.TH (mkFromHttpInstanceForEnum)
import Tools.Beam.UtilsTH (mkBeamInstancesForEnum)

data Category = CAR | MOTORCYCLE | TRAIN | PUBLIC_TRANSPORT_BUS | FLIGHT | AUTO_CATEGORY
  deriving stock (Show, Eq, Read, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema, ToParamSchema)
  deriving (PrettyShow) via Showable Category

$(mkBeamInstancesForEnum ''Category)

$(mkFromHttpInstanceForEnum ''Category)

----
data RegistrationCategory = COMMERCIAL | PERSONAL | OTHER | PUBLIC
  deriving stock (Show, Eq, Read, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)
  deriving (PrettyShow) via Showable RegistrationCategory

$(mkBeamInstancesForEnum ''RegistrationCategory)

$(mkFromHttpInstanceForEnum ''RegistrationCategory)

data Vehicle = Vehicle
  { driverId :: Id DPers.Person,
    merchantId :: Id DM.Merchant,
    variant :: Reexport.Variant,
    model :: Text,
    color :: Text,
    vehicleName :: Maybe Text,
    registrationNo :: Text,
    capacity :: Maybe Int,
    category :: Maybe Category,
    make :: Maybe Text,
    size :: Maybe Text,
    energyType :: Maybe Text,
    registrationCategory :: Maybe RegistrationCategory,
    vehicleClass :: Text,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, Show, Eq, PrettyShow)

data VehicleAPIEntity = VehicleAPIEntity
  { driverId :: Id DPers.Person,
    variant :: Reexport.Variant,
    model :: Text,
    color :: Text,
    vehicleName :: Maybe Text,
    registrationNo :: Text,
    category :: Maybe Category,
    capacity :: Maybe Int,
    createdAt :: UTCTime
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

makeVehicleAPIEntity :: Vehicle -> VehicleAPIEntity
makeVehicleAPIEntity Vehicle {..} = VehicleAPIEntity {..}

getCategory :: Reexport.Variant -> Category
getCategory SEDAN = CAR
getCategory SUV = CAR
getCategory HATCHBACK = CAR
getCategory AUTO_RICKSHAW = AUTO_CATEGORY
getCategory TAXI = CAR
getCategory TAXI_PLUS = CAR
getCategory BUS = PUBLIC_TRANSPORT_BUS
