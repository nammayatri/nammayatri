module Mobility.AppBackend.Fixtures where

import "app-backend" Domain.Types.Merchant as DM
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Types.Version

appRegistrationToken :: Text
appRegistrationToken = "ea37f941-427a-4085-a7d0-96240f166672"

appRegistrationToken2 :: Text
appRegistrationToken2 = "003df941-427a-4085-a7d0-96240f166672"

defaultVersion :: Version
defaultVersion = Version 0 0 0

yatriMerchantId :: Id DM.Merchant
yatriMerchantId = "da4e23a5-3ce6-4c37-8b9b-41377c3c1a51"
