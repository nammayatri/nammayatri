module Fixtures.Person where

import Beckn.External.Encryption
import Beckn.Types.Id
import Database.Beam (Nullable)
import EulerHS.Prelude
import qualified Fixtures.Time as Fixtures
import qualified Types.Storage.Person as Person

defaultDriver :: Person.Person
defaultDriver =
  Person.Person
    { id = Id "1",
      firstName = Just "Driver",
      middleName = Nothing,
      lastName = Just "Driverson",
      fullName = Nothing,
      role = Person.DRIVER,
      gender = Person.UNKNOWN,
      identifierType = Person.EMAIL,
      email = Just "driverson@cool-drivers.com",
      mobileNumber = EncryptedHashed {encrypted = Nothing, hash = Nothing},
      mobileCountryCode = Nothing,
      passwordHash = Nothing,
      identifier = Nothing,
      rating = Nothing,
      verified = False,
      udf1 = Nothing,
      udf2 = Nothing,
      status = Person.ACTIVE,
      organizationId = Nothing,
      locationId = Nothing,
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
