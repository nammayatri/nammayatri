{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.CancellationCharges where

import qualified Domain.Types.Person as Person
import qualified Domain.Types.Ride as Ride
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common (HighPrecMoney)

data CancellationCharges = CancellationCharges
  { id :: Id CancellationCharges,
    driverId :: Id Person.Person,
    rideId :: Maybe (Id Ride.Ride),
    cancellationCharges :: HighPrecMoney
  }
  deriving (Generic)
