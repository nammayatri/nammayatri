{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Mobility.AppBackend.Fixtures where

import "rider-app" Domain.Types.Merchant as DM
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Types.Version

appRegistrationToken :: Text
appRegistrationToken = "ea37f941-427a-4085-a7d0-96240f166672"

appRegistrationToken2 :: Text
appRegistrationToken2 = "003df941-427a-4085-a7d0-96240f166672"

defaultVersion :: Version
defaultVersion = Version 0 0 0 Nothing Nothing

yatriMerchantId :: Id DM.Merchant
yatriMerchantId = "da4e23a5-3ce6-4c37-8b9b-41377c3c1a51"
