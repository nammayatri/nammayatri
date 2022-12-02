module API.Dashboard.Customer where

import Beckn.Prelude
import Beckn.Types.Id
import Beckn.Utils.Common
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import Environment
import Servant hiding (throwError)
import qualified Storage.CachedQueries.Merchant as QM
import Tools.Error

type API =
  "customer"
    :> ( CustomerListAPI
           :<|> CustomerUpdateAPI
       )

type CustomerListAPI =
  "list"
    :> QueryParam "limit" Integer
    :> QueryParam "offset" Integer
    :> Get '[JSON] Text

type CustomerUpdateAPI =
  Capture "customerId" (Id DP.Person)
    :> "update"
    :> ReqBody '[JSON] Text -- DProfile.UpdateProfileReq
    :> Post '[JSON] Text -- APISuccess

handler :: ShortId DM.Merchant -> FlowServer API
handler merchantId =
  listCustomer merchantId
    :<|> updateCustomer merchantId

listCustomer ::
  ShortId DM.Merchant ->
  Maybe Integer ->
  Maybe Integer ->
  FlowHandler Text
listCustomer merchantShortId _ _ = withFlowHandlerAPI $ do
  _merchant <-
    QM.findByShortId merchantShortId
      >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  pure "To be done"

updateCustomer ::
  ShortId DM.Merchant ->
  Id DP.Person ->
  Text ->
  FlowHandler Text
updateCustomer merchantShortId _personId _req = withFlowHandlerAPI $ do
  _merchant <-
    QM.findByShortId merchantShortId
      >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  pure "To be done"
