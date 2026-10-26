module Lib.BmsBackend.API.Events where

import qualified EulerHS.Types as ET
import Kernel.Prelude
import Lib.BmsBackend.Types
import Servant

type ReportEventAPI =
  "events"
    :> ReqBody '[JSON] ReportEventReq
    :> Post '[JSON] NoContent

reportEventAPI :: Proxy ReportEventAPI
reportEventAPI = Proxy

reportEvent :: ReportEventReq -> ET.EulerClient NoContent
reportEvent = ET.client reportEventAPI
