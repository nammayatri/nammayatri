module Mobility.ARDU.Fixtures where

import Beckn.Prelude
import Beckn.Types.Common (Money)
import Beckn.Types.Id
import Beckn.Types.Time
import "driver-offer-bpp" Domain.Types.Merchant as DM
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

defaultAllowedDriverFee :: Money
defaultAllowedDriverFee = 30

nammaYatriPartnerMerchantId :: Id DM.Merchant
nammaYatriPartnerMerchantId = "favorit0-0000-0000-0000-00000favorit"

nammaYatriPartnerMerchantShortId :: ShortId DM.Merchant
nammaYatriPartnerMerchantShortId = "NAMMA_YATRI_PARTNER"

otherMerchant2Id :: Id DM.Merchant
otherMerchant2Id = "nearest-drivers-testing-organization"

dashboardToken :: Text
dashboardToken = "some-secret-dashboard-token-for-driver-offer-bpp"
