module API.Dashboard where

import App.Types
import Beckn.Prelude
import Beckn.Types.Id
import Beckn.Utils.Common
import qualified Domain.Types.Merchant as DMerchant
import Servant hiding (throwError)
import qualified Storage.Queries.Merchant as QMerchant
import Types.Error
import Utils.Auth (Dashboard, DashboardTokenAuth)

-- TODO do we need different tokens for different merchants? now we have one common token

type API =
  "dashboard"
    :> Capture "merchantId" (ShortId DMerchant.Merchant)
    :> DashboardTokenAuth
    :> CustomerListAPI

type CustomerListAPI =
  "customer"
    :> "list"
    :> QueryParam "limit" Integer
    :> QueryParam "offset" Integer
    :> Get '[JSON] Text

handler :: FlowServer API
handler =
  listCustomer

listCustomer ::
  ShortId DMerchant.Merchant ->
  Dashboard ->
  Maybe Integer ->
  Maybe Integer ->
  FlowHandler Text
listCustomer merchantShortId _ _ _ = withFlowHandlerAPI $ do
  _merhant <-
    QMerchant.findByShortId merchantShortId
      >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  pure "To be done"
