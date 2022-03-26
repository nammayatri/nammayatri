module Domain.Person where

import Beckn.Prelude
import Beckn.Types.Id

data Person = Person
  { id :: Id Person,
    name :: Text,
    phone :: Text
  }
