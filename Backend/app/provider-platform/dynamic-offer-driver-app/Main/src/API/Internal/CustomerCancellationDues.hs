module API.Internal.CustomerCancellationDues
  ( API,
    handler,
  )
where

import qualified Domain.Action.Internal.CustomerCancellationDues as Domain
import Domain.Types.Merchant (Merchant)
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Types.APISuccess
import Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant

type API =
  Capture "merchantId" (Id Merchant)
    :> Capture "merchantCity" Context.City
    :> "dispute"
    :> Header "token" Text
    :> ReqBody '[JSON] Domain.CancellationDuesReq
    :> Post '[JSON] APISuccess
    :<|> Capture "merchantId" (Id Merchant)
      :> Capture "merchantCity" Context.City
      :> "getCancellationDuesDetails"
      :> Header "token" Text
      :> ReqBody '[JSON] Domain.CancellationDuesReq
      :> Get '[JSON] Domain.CancellationDuesDetailsRes
    :<|> Capture "merchantId" (Id Merchant)
      :> Capture "merchantCity" Context.City
      :> "customerCancellationDuesSync"
      :> Header "token" Text
      :> ReqBody '[JSON] Domain.CustomerCancellationDuesSyncReq
      :> Post '[JSON] APISuccess

handler :: FlowServer API
handler =
  disputeCancellationDues
    :<|> getCancellationDuesDetails
    :<|> customerCancellationDuesSync

disputeCancellationDues :: Id Merchant -> Context.City -> Maybe Text -> Domain.CancellationDuesReq -> FlowHandler APISuccess
disputeCancellationDues merchantId merchantCity apiKey = withFlowHandlerAPI . Domain.disputeCancellationDues merchantId merchantCity apiKey

getCancellationDuesDetails :: Id Merchant -> Context.City -> Maybe Text -> Domain.CancellationDuesReq -> FlowHandler Domain.CancellationDuesDetailsRes
getCancellationDuesDetails merchantId merchantCity apiKey = withFlowHandlerAPI . Domain.getCancellationDuesDetails merchantId merchantCity apiKey

customerCancellationDuesSync :: Id Merchant -> Context.City -> Maybe Text -> Domain.CustomerCancellationDuesSyncReq -> FlowHandler APISuccess
customerCancellationDuesSync merchantId merchantCity apiKey = withFlowHandlerAPI . Domain.customerCancellationDuesSync merchantId merchantCity apiKey
