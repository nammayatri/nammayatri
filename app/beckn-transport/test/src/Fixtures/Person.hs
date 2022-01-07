module Fixtures.Person where

import Beckn.Types.Id
import qualified Domain.Types.Person as Person
import EulerHS.Prelude
import qualified Fixtures.Time as Fixtures

defaultDriver :: Person.Person
defaultDriver =
  Person.Person
    { id = Id "1",
      firstName = Just "Driver",
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
      udf1 = Nothing,
      udf2 = Nothing,
      organizationId = Nothing,
      deviceToken = Nothing,
      description = Nothing,
      createdAt = Fixtures.defaultTime,
      updatedAt = Fixtures.defaultTime
    }

defaultAdmin :: Person.Person
defaultAdmin =
  defaultDriver
    { Person.id = Id "admin",
      Person.firstName = Just "Admin",
      Person.lastName = Just "Adminson",
      Person.role = Person.ADMIN,
      Person.email = Just "adminson@cool-admins.com"
    }
