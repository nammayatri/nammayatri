module API.Dashboard.Driver.Registration where

import Beckn.Types.Id
import qualified "dashboard-bpp-helper-api" Dashboard.Common.Driver.Registration as Common
import Domain.Action.Dashboard.Driver.Registration
import qualified Domain.Types.Merchant as DM
import Environment
import Servant

type API =
  Common.DocumentsListAPI
    :<|> Common.GetDocumentAPI
    :<|> Common.UploadDocumentAPI
    :<|> Common.RegisterDLAPI
    :<|> Common.RegisterRCAPI

handler :: ShortId DM.Merchant -> FlowServer API
handler merchantId =
  documentsList merchantId
    :<|> getDocument merchantId
    :<|> uploadDocument merchantId
    :<|> registerDL merchantId
    :<|> registerRC merchantId
