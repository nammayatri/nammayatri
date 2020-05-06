module Beckn.Product.Customer where

import qualified Beckn.Data.Accessor                  as Accessor
import qualified Beckn.Storage.Queries.Customer       as QC
import qualified Beckn.Storage.Queries.CustomerDetail as CustomerDetail
import           Beckn.Types.API.Customer
import           Beckn.Types.App
import           Beckn.Types.Common
import           Beckn.Types.Storage.Customer
import           Beckn.Types.Storage.CustomerDetail   as CustomerDetail
import           Beckn.Utils.Common                   (fromMaybeM400)
import           Beckn.Utils.Routes
import           Beckn.Utils.Storage
import           Data.Aeson
import qualified Data.Text                            as T
import qualified EulerHS.Language                     as L
import           EulerHS.Prelude                      hiding (mask)
import           Servant

getCustomerInfo ::
  Maybe Text -> Text -> FlowHandler GetCustomerRes
getCustomerInfo regToken customerId = withFlowHandler $ do
  reg <- verifyToken regToken
  customer <- fromMaybeM400 "INVALID_DATA"
              =<< QC.findCustomerById (CustomerId customerId)
  custDetails <- CustomerDetail.findAllByCustomerId (CustomerId customerId)
  return $ GetCustomerRes customer (sanitizeDetails <$> custDetails)

mask :: Text -> Text
mask txt = let
  prefixLen = length txt - 4
  last4 = T.reverse $ T.take 4 $ T.reverse txt
  in (T.replicate prefixLen "X") <> last4

sanitizeDetails :: CustomerDetail -> AdditionalInfo
sanitizeDetails CustomerDetail{..} =
  AdditionalInfo
  { _uniqueIdentifier = if shouldSanitize then mask _uniqueIdentifier else _uniqueIdentifier
  , _value = Json _value
  , ..
  }
  where
    shouldSanitize =
      case _identifierType of
        CustomerDetail.MOBILENUMBER -> False
        CustomerDetail.AADHAAR      -> True
