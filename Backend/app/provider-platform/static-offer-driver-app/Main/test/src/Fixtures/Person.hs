 {-
 Copyright 2022-23, Juspay India Pvt Ltd
 
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License 
 
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program 
 
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY 
 
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of 
 
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Fixtures.Person where

import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as Person
import EulerHS.Prelude
import qualified Fixtures.Time as Fixtures
import Kernel.Types.Id
import Kernel.Types.Version

defaultVersion :: Version
defaultVersion = Version 0 0 0

defaultMerchantId :: Id DM.Merchant
defaultMerchantId = Id "merchant1"

defaultDriver :: Person.Person
defaultDriver =
  Person.Person
    { id = Id "1",
      firstName = "Driver",
      middleName = Nothing,
      lastName = Just "Driverson",
      role = Person.DRIVER,
      gender = Person.UNKNOWN,
      identifierType = Person.EMAIL,
      email = Just "driverson@cool-drivers.com",
      mobileNumber = Nothing,
      mobileCountryCode = Nothing,
      passwordHash = Nothing,
      identifier = Nothing,
      rating = Nothing,
      isNew = True,
      merchantId = defaultMerchantId,
      deviceToken = Nothing,
      description = Nothing,
      createdAt = Fixtures.defaultTime,
      updatedAt = Fixtures.defaultTime,
      bundleVersion = Just defaultVersion,
      clientVersion = Just defaultVersion
    }

anotherDriver :: Person.Person
anotherDriver =
  defaultDriver
    { Person.id = Id "anotherDriver"
    }

defaultAdmin :: Person.Person
defaultAdmin =
  defaultDriver
    { Person.id = Id "admin",
      Person.firstName = "Admin",
      Person.lastName = Just "Adminson",
      Person.role = Person.ADMIN,
      Person.email = Just "adminson@cool-admins.com"
    }

anotherMerchantId :: Id DM.Merchant
anotherMerchantId = Id "anotherMerchantId"

anotherMerchantAdmin :: Person.Person
anotherMerchantAdmin =
  defaultAdmin
    { Person.id = Id "anotherMerchantAdmin",
      Person.merchantId = anotherMerchantId
    }
