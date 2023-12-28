{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Mobility.ARDU.Fixtures where

import "dynamic-offer-driver-app" Domain.Types.Merchant as DM
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common (Money)
import Kernel.Types.Id
import Kernel.Types.Time
import Utils

timeBetweenLocationUpdates :: Seconds
timeBetweenLocationUpdates = 1

bapTransporterName :: Text
bapTransporterName = "Driver-Offer-Provider #1"

arduDriver1 :: DriverTestData
arduDriver1 =
  DriverTestData
    { driverId = Id "favorit-auto1-0000000000000000000000",
      token = "favorit-auto1-0000000000000000-token"
    }

arduDriver2 :: DriverTestData
arduDriver2 =
  DriverTestData
    { driverId = Id "favorit-auto2-0000000000000000000000",
      token = "favorit-auto2-0000000000000000-token"
    }

-- See restricted_extra_fare
defaultAllowedDriverFee :: Money
defaultAllowedDriverFee = 10

nammaYatriPartnerMerchantId :: Id DM.Merchant
nammaYatriPartnerMerchantId = "favorit0-0000-0000-0000-00000favorit"

nammaYatriPartnerMerchantShortId :: ShortId DM.Merchant
nammaYatriPartnerMerchantShortId = "NAMMA_YATRI_PARTNER"

nammaYatriDefaultOperatingCity :: Context.City
nammaYatriDefaultOperatingCity = Context.Bangalore

otherMerchant2Id :: Id DM.Merchant
otherMerchant2Id = "nearest-drivers-testing-organization"

dashboardToken :: Text
dashboardToken = "some-secret-dashboard-token-for-driver-offer-bpp"
