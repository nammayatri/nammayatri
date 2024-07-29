{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Dashboard.ProviderPlatform.RideBooking.Driver
  ( module Reexport,
  )
where

import API.Types.ProviderPlatform.RideBooking.Driver as Reexport
import Dashboard.Common as Reexport
import Dashboard.Common.Driver as Reexport
import qualified Kernel.Storage.ClickhouseV2 as CH

instance CH.ClickhouseValue DriverFeeStatus
