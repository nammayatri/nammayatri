module API.UI.DriverOnboarding where

import Beckn.Types.Id
import Beckn.Utils.Common
import qualified Domain.Action.UI.DriverOnboarding.DriverLicense as DriverOnboarding
import qualified Domain.Action.UI.DriverOnboarding.Image as Image
import qualified Domain.Action.UI.DriverOnboarding.Referral as DriverOnboarding
import qualified Domain.Action.UI.DriverOnboarding.Status as DriverOnboarding
import qualified Domain.Action.UI.DriverOnboarding.VehicleRegistrationCertificate as DriverOnboarding
import qualified Domain.Types.Person as DP
import Environment
import EulerHS.Prelude
import Servant
import Tools.Auth (AdminTokenAuth, TokenAuth)

type API =
  "driver" :> "register"
    :> ( "dl"
           :> TokenAuth
           :> ReqBody '[JSON] DriverOnboarding.DriverDLReq
           :> Post '[JSON] DriverOnboarding.DriverDLRes
           :<|> "rc"
             :> TokenAuth
             :> ReqBody '[JSON] DriverOnboarding.DriverRCReq
             :> Post '[JSON] DriverOnboarding.DriverRCRes
           :<|> "status"
             :> TokenAuth
             :> Get '[JSON] DriverOnboarding.StatusRes
           :<|> "validateImage"
             :> TokenAuth
             :> ReqBody '[JSON] Image.ImageValidateRequest
             :> Post '[JSON] Image.ImageValidateResponse
       )
    :<|> "driver" :> "referral"
      :> TokenAuth
      :> ReqBody '[JSON] DriverOnboarding.ReferralReq
      :> Post '[JSON] DriverOnboarding.ReferralRes
    :<|> "driver" :> "getDocs" -- FIXME: Temporary API will move to dashboard later
      :> AdminTokenAuth
      :> MandatoryQueryParam "mobileNumber" Text
      :> Get '[JSON] Image.GetDocsResponse

handler :: FlowServer API
handler =
  ( verifyDL
      :<|> verifyRC
      :<|> statusHandler
      :<|> validateImage
  )
    :<|> addReferral
    :<|> getDocs

verifyDL :: Id DP.Person -> DriverOnboarding.DriverDLReq -> FlowHandler DriverOnboarding.DriverDLRes
verifyDL personId = withFlowHandlerAPI . DriverOnboarding.verifyDL False personId

verifyRC :: Id DP.Person -> DriverOnboarding.DriverRCReq -> FlowHandler DriverOnboarding.DriverRCRes
verifyRC personId = withFlowHandlerAPI . DriverOnboarding.verifyRC False personId

statusHandler :: Id DP.Person -> FlowHandler DriverOnboarding.StatusRes
statusHandler = withFlowHandlerAPI . DriverOnboarding.statusHandler

validateImage :: Id DP.Person -> Image.ImageValidateRequest -> FlowHandler Image.ImageValidateResponse
validateImage personId = withFlowHandlerAPI . Image.validateImage False personId

addReferral :: Id DP.Person -> DriverOnboarding.ReferralReq -> FlowHandler DriverOnboarding.ReferralRes
addReferral personId = withFlowHandlerAPI . DriverOnboarding.addReferral personId

getDocs :: DP.Person -> Text -> FlowHandler Image.GetDocsResponse
getDocs person = withFlowHandlerAPI . Image.getDocs person
