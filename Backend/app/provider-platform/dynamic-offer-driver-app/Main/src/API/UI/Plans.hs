module API.UI.Plans where

import Data.OpenApi (ToSchema)
import qualified Domain.Action.UI.Payment as DPayment
import Domain.Types.DriverFee
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.Person as DP
import Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.External.Payment.Interface as Payment
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Payment.API as Payment
import qualified Lib.Payment.Domain.Types.PaymentOrder as DOrder
import Servant
import Tools.Auth

data Plans = Plans
  { name :: Text
  }
  deriving (Generic, ToJSON, ToSchema)

type Api =
  TokenAuth
    :> ( "plans"
           :> Get '[JSON] Plans
           :<|> Capture "planId"
           :> "subscription"
           :> Post '[JSON] Payment.CreateOrderResp
       )
