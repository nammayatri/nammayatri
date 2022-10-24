module API.Dashboard.Driver.Registration where

import qualified "dashboard-bpp-helper-api" Dashboard.Common.Driver.Registration as Common
import Domain.Action.Dashboard.Driver.Registration
import Environment
import Servant
import Tools.Auth (Dashboard)

type API =
  Common.DocumentsListAPI
    :<|> Common.GetDocumentAPI
    :<|> Common.UploadDocumentAPI
    :<|> Common.RegisterDLAPI
    :<|> Common.RegisterRCAPI

handler :: Dashboard -> FlowServer API
handler _ =
  documentsList
    :<|> getDocument
    :<|> uploadDocument
    :<|> registerDL
    :<|> registerRC
