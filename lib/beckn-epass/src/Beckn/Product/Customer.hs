module Beckn.Product.Customer where

import qualified Beckn.Data.Accessor      as Accessor
import           Beckn.Types.API.Customer
import           Beckn.Types.App
import           Beckn.Types.Common
import qualified Beckn.Storage.Queries.Customer as QC
import           Beckn.Utils.Routes
import           Beckn.Utils.Storage
import           Data.Aeson
import           EulerHS.Prelude
import qualified EulerHS.Language as L
import           Servant

getCustomerInfo ::
  Maybe Text -> Text -> FlowHandler GetCustomerRes
getCustomerInfo regToken customerId = withFlowHandler $ do
  reg <- verifyToken regToken
  QC.findCustomerById (CustomerId customerId) >>=
    maybe
      (L.throwException $ err400 {errBody = "INVALID_DATA"})
      (return . GetCustomerRes)
