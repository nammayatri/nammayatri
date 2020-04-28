module Beckn.Product.Customer where

import qualified Beckn.Data.Accessor      as Accessor
import           Beckn.Types.API.Common
import           Beckn.Types.API.Customer
import           Beckn.Types.App
import           Data.Aeson
import           EulerHS.Prelude

getCustomerInfo ::
  Maybe Text -> Text -> FlowHandler GetCustomerRes
getCustomerInfo regToken customerId = undefined
