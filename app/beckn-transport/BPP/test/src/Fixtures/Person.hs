module Fixtures.Person where

import Beckn.Types.Id
import Beckn.Types.Version
import qualified Domain.Types.Person as Person
import EulerHS.Prelude
import qualified Fixtures.Time as Fixtures

defaultVersion :: Version
defaultVersion = Version 0 0 0

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
      merchantId = Id "1",
      deviceToken = Nothing,
      description = Nothing,
      createdAt = Fixtures.defaultTime,
      updatedAt = Fixtures.defaultTime,
      bundleVersion = Just defaultVersion,
      clientVersion = Just defaultVersion
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
