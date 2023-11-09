{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.RentalDetails where

import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.JSON (removeNullFields)

data RentalDetails = RentalDetails
  { id :: Id RentalDetails,
    baseFare :: Money,
    perHourCharge :: Money,
    perHourFreeKms :: Int,
    perExtraKmRate :: Money,
    nightShiftInfo :: Maybe NightShiftInfo
  }
  deriving (Generic, Show)

data NightShiftInfo = NightShiftInfo
  { nightShiftCharge :: Money,
    oldNightShiftCharge :: Maybe Centesimal,
    nightShiftStart :: TimeOfDay,
    nightShiftEnd :: TimeOfDay
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data RentalDetailsAPIEntity = RentalDetailsAPIEntity
  { baseDuration :: Maybe Hours, -- Nothing for quoteDetails, Just for bookingDetails
    baseFare :: Money,
    perHourCharge :: Money,
    perHourFreeKms :: Int,
    perExtraKmRate :: Money,
    nightShiftInfo :: Maybe NightShiftInfo
  }
  deriving (Generic, FromJSON, Show, ToSchema)

instance ToJSON RentalDetailsAPIEntity where
  toJSON = genericToJSON removeNullFields
