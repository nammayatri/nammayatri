module Mobility.Transporter.Fixtures where

import Beckn.Types.Id
import Beckn.Types.Time (Seconds)
import "beckn-transport" Domain.Types.Merchant as DM
import qualified "app-backend" Domain.Types.Person as TPerson
import EulerHS.Prelude
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
