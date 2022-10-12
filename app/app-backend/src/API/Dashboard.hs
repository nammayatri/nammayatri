module API.Dashboard where

import Beckn.Prelude
import Beckn.Types.Id
import Beckn.Utils.Common
import qualified Domain.Types.Merchant as DMerchant
import Environment
import Servant hiding (throwError)
import qualified Storage.CachedQueries.Merchant as QMerchant
import Tools.Auth (Dashboard, DashboardTokenAuth)
import Tools.Error

type API =
  Capture "merchantId" (ShortId DMerchant.Merchant)
    :> API'

-- TODO do we need different tokens for different merchants? now we have one common token
type API' =
  "dashboard"
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
