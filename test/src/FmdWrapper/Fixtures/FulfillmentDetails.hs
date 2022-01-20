module FmdWrapper.Fixtures.FulfillmentDetails where

import EulerHS.Prelude
import qualified FmdWrapper.Fixtures.Contact as Fixtures
import qualified FmdWrapper.Fixtures.Location as Fixtures
import qualified FmdWrapper.Fixtures.Person as Fixtures
import "fmd-wrapper" Types.Beckn.FulfillmentDetails (FulfillmentDetails (..))

startFulfillmentDetails :: FulfillmentDetails
startFulfillmentDetails =
  FulfillmentDetails
    { location = Fixtures.startLocation,
      instructions = Nothing,
      contact = Fixtures.contact,
      person = Fixtures.person
    }

endFulfillmentDetails :: FulfillmentDetails
endFulfillmentDetails =
  FulfillmentDetails
    { location = Fixtures.endLocation,
      instructions = Nothing,
      contact = Fixtures.contact,
      person = Fixtures.person
    }
