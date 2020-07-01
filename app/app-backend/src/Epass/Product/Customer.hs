module Epass.Product.Customer where

import Beckn.Types.Storage.Person
import qualified Beckn.Types.Storage.Person as Person
import Beckn.Utils.Common
import Data.Aeson
import qualified Data.Text as T
import qualified Epass.Data.Accessor as Accessor
import Epass.Types.API.Customer
import Epass.Types.App
import Epass.Types.Common
import Epass.Utils.Storage
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (mask)
import Servant
import qualified Storage.Queries.Person as QP

getCustomerInfo ::
  RegToken -> Text -> FlowHandler GetCustomerRes
getCustomerInfo regToken customerId = withFlowHandler $ do
  reg <- verifyToken regToken
  customer <-
    fromMaybeM400 "INVALID_DATA"
      =<< QP.findById (PersonId customerId)
  return $ GetCustomerRes $ sanitizeDetails customer

mask :: Maybe Text -> Maybe Text
mask txtM =
  let prefixLen txt = length txt - 4
      last4 txt = T.reverse $ T.take 4 $ T.reverse txt
   in (\t -> T.replicate (prefixLen t) "X" <> last4 t) <$> txtM

sanitizeDetails :: Person -> Person
sanitizeDetails person =
  person
    { _identifier = if shouldSanitize then mask uniqueIdentifier else uniqueIdentifier
    }
  where
    uniqueIdentifier = _identifier person
    shouldSanitize =
      case _identifierType person of
        Person.MOBILENUMBER -> False
        Person.AADHAAR -> True
