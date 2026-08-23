module API.Beckn.FRFSSeller
  ( API,
    handler,
  )
where

import qualified API.Beckn.FRFSSeller.Cancel as Cancel
import qualified API.Beckn.FRFSSeller.Confirm as Confirm
import qualified API.Beckn.FRFSSeller.IGM as IGM
import qualified API.Beckn.FRFSSeller.Init as Init
import qualified API.Beckn.FRFSSeller.Search as Search
import qualified API.Beckn.FRFSSeller.Select as Select
import qualified API.Beckn.FRFSSeller.Status as Status
import qualified Data.Text as T
import Environment
import Kernel.Prelude (Text)
import qualified Kernel.Types.Beckn.Domain as Domain
import Servant hiding (throwError)
import Tools.SellerSignatureAuth (SellerSignatureAuth)

type API =
  Capture "operator" Text
    :> "metro"
    :> "seller"
    :> SellerSignatureAuth 'Domain.PUBLIC_TRANSPORT "Authorization"
    :> ( Search.API
           :<|> Select.API
           :<|> Init.API
           :<|> Confirm.API
           :<|> Status.API
           :<|> Cancel.API
           :<|> IGM.API
       )

handler :: FlowServer API
handler operator auth =
  Search.handler op auth
    :<|> Select.handler op auth
    :<|> Init.handler op auth
    :<|> Confirm.handler op auth
    :<|> Status.handler op auth
    :<|> Cancel.handler op auth
    :<|> IGM.handler op auth
  where
    op = T.toLower operator
