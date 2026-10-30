module SharedLogic.External.Umis.API where

import qualified EulerHS.Types as ET
import Kernel.Prelude
import Servant
import SharedLogic.External.Umis.Types

type StudentDetailsAPI =
  "api"
    :> "ADWD"
    :> "ChennaiOneBusPass"
    :> Capture "umisId" Text
    :> Header "Authorization" Text
    :> Get '[JSON] UmisStudentResp

umisStudentDetailsAPI :: Proxy StudentDetailsAPI
umisStudentDetailsAPI = Proxy

getUmisStudentDetails :: Text -> Maybe Text -> ET.EulerClient UmisStudentResp
getUmisStudentDetails = ET.client umisStudentDetailsAPI
