module FmdWrapper.Fixtures.Person where

import qualified FmdWrapper.Fixtures.Name as Fixtures
import "fmd-wrapper" Types.Beckn.Person (Person (..))

person :: Person
person = Person {name = Fixtures.name}
