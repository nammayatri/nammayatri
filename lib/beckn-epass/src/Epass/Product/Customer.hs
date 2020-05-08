module Epass.Product.Customer where

import qualified Epass.Data.Accessor      as Accessor
import           Epass.Types.API.Customer
import           Epass.Types.App
import           Epass.Types.Common
import qualified Epass.Storage.Queries.Customer as QC
import           Epass.Utils.Routes
import           Epass.Utils.Storage
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
