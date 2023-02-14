 {-
 Copyright 2022-23, Juspay India Pvt Ltd
 
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License 
 
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program 
 
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY 
 
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of 
 
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Mobility.Transporter.Fixtures where

import "static-offer-driver-app" Domain.Types.Merchant as DM
import qualified "rider-app" Domain.Types.Person as TPerson
import EulerHS.Prelude
import Kernel.Types.Id
import Kernel.Types.Time (Seconds)
import Utils

timeBetweenLocationUpdates :: Seconds
timeBetweenLocationUpdates = 1

waitBetweenUpdates :: Int
waitBetweenUpdates = 1e5 + 1e6 * fromIntegral timeBetweenLocationUpdates

bapTransporterName :: Text
bapTransporterName = "[A] Transporter #1"

transporterDriver1 :: DriverTestData
transporterDriver1 =
  DriverTestData
    { driverId = Id "6bc4bc84-2c43-425d-8853-22f47driver1",
      token = "ca05cf3c-c88b-4a2f-8874-drivertoken1"
    }

transporterDriver2 :: DriverTestData
transporterDriver2 =
  DriverTestData
    { driverId = "6bc4bc84-2c43-425d-8853-22f47driver2",
      token = "ca05cf3c-c88b-4a2f-8874-drivertoken2"
    }

testVehicleId :: Text
testVehicleId = "0c1cd0bc-b3a4-4c6c-811f-900ccf4dfb94"

driverToken1 :: Text
driverToken1 = "ca05cf3c-c88b-4a2f-8874-drivertoken1"

driverToken2 :: Text
driverToken2 = "ca05cf3c-c88b-4a2f-8874-drivertoken2"

testDriverId1 :: Id TPerson.Person
testDriverId1 = Id "6bc4bc84-2c43-425d-8853-22f47driver1"

testDriverId2 :: Id TPerson.Person
testDriverId2 = Id "6bc4bc84-2c43-425d-8853-22f47driver2"

yatriPartnerMerchantId :: Id DM.Merchant
yatriPartnerMerchantId = "7f7896dd-787e-4a0b-8675-e9e6fe93bb8f"

otherMerchantId :: Id DM.Merchant
otherMerchantId = "e1f37274-f0aa-4bb3-93a0-2476349487b7"
