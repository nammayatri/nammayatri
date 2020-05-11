module Epass.Product.Customer where

import Data.Aeson
import qualified Data.Text as T
import qualified Epass.Data.Accessor as Accessor
import qualified Epass.Storage.Queries.Customer as QC
import qualified Epass.Storage.Queries.CustomerDetail as CustomerDetail
import Epass.Types.API.Customer
import Epass.Types.App
import Epass.Types.Common
import Epass.Types.Storage.CustomerDetail as CustomerDetail
import Epass.Utils.Common
import Epass.Utils.Routes
import Epass.Utils.Storage
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (mask)
import Servant

getCustomerInfo ::
  Maybe Text -> Text -> FlowHandler GetCustomerRes
getCustomerInfo regToken customerId = withFlowHandler $ do
  reg <- verifyToken regToken
  customer <-
    fromMaybeM400 "INVALID_DATA"
      =<< QC.findCustomerById (CustomerId customerId)
  custDetails <- CustomerDetail.findAllByCustomerId (CustomerId customerId)
  return $ GetCustomerRes customer (sanitizeDetails <$> custDetails)

mask :: Text -> Text
mask txt =
  let prefixLen = length txt - 4
      last4 = T.reverse $ T.take 4 $ T.reverse txt
   in (T.replicate prefixLen "X") <> last4

sanitizeDetails :: CustomerDetail -> AdditionalInfo
sanitizeDetails CustomerDetail {..} =
  AdditionalInfo
    { _uniqueIdentifier = if shouldSanitize then mask _uniqueIdentifier else _uniqueIdentifier,
      _value = Json _value,
      ..
    }
  where
    shouldSanitize =
      case _identifierType of
        CustomerDetail.MOBILENUMBER -> False
        CustomerDetail.AADHAAR -> True
