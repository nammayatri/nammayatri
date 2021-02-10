module Fixtures.Person (defaultDriver) where

import Beckn.Types.App (PersonId (..))
import qualified Beckn.Types.Storage.Person as Person
import EulerHS.Prelude
import qualified Fixtures.Time as Fixtures

defaultDriver :: Person.Person
defaultDriver =
  Person.Person
    { _id = PersonId "1",
      _firstName = Just "Driver",
      _middleName = Nothing,
      _lastName = Just "Driverson",
      _fullName = Nothing,
      _role = Person.DRIVER,
      _gender = Person.UNKNOWN,
      _identifierType = Person.EMAIL,
      _email = Just "driverson@cool-drivers.com",
      _mobileNumber = Nothing,
      _mobileCountryCode = Nothing,
      _passwordHash = Nothing,
      _identifier = Nothing,
      _rating = Nothing,
      _verified = False,
      _udf1 = Nothing,
      _udf2 = Nothing,
      _status = Person.ACTIVE,
      _organizationId = Nothing,
      _locationId = Nothing,
      _deviceToken = Nothing,
      _description = Nothing,
      _createdAt = Fixtures.defaultTime,
      _updatedAt = Fixtures.defaultTime
    }
