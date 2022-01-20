module FmdWrapper.Fixtures.API.Confirm where

import Beckn.Utils.Example (example)
import EulerHS.Prelude
import qualified FmdWrapper.Fixtures.FulfillmentDetails as Fixtures
import qualified FmdWrapper.Fixtures.Order as Fixtures
import qualified "fmd-wrapper" Types.Beckn.API.Confirm as ConfirmAPI

confirmOrderObject :: ConfirmAPI.OrderObject
confirmOrderObject =
  ConfirmAPI.OrderObject $
    ConfirmAPI.Order
      { items = [Fixtures.orderItem],
        billing = example,
        fulfillment = fulfillment,
        payment = payment
      }

payment :: ConfirmAPI.Payment
payment =
  ConfirmAPI.Payment
    { params =
        ConfirmAPI.Params
          { transaction_id = "transaction id",
            amount = Nothing,
            currency = "INR"
          }
    }

fulfillment :: ConfirmAPI.Fulfillment
fulfillment =
  ConfirmAPI.Fulfillment
    { tracking = False,
      start = Fixtures.startFulfillmentDetails,
      end = Fixtures.endFulfillmentDetails
    }
