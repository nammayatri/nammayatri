module API.UI.DriverOnboarding
  ( DriverOnboarding.DriverDLImageReq (..),
    DriverOnboarding.DriverDLReq (..),
    DriverOnboarding.DriverDLRes,
    DriverOnboarding.IdfyDLReq (..),
    DriverOnboarding.DLVerificationResult (..),
    DriverOnboarding.SourceOutput (..),
    DriverOnboarding.CovDetails (..),
    DriverOnboarding.IdfyRCReq (..),
    DriverOnboarding.RCVerificationResult (..),
    DriverOnboarding.ExtractionOutput (..),
    DriverOnboarding.ResponseStatus (..),
    DriverOnboarding.StatusRes (..),
    DriverOnboarding.DriverRCImageReq (..),
    DriverOnboarding.DriverRCReq (..),
    DriverOnboarding.DriverRCRes,
    API,
    handler,
  )
where

import Beckn.Types.Id
import Beckn.Utils.Common
import qualified Domain.Action.UI.DriverOnboarding.DriverLicense as DriverOnboarding
import qualified Domain.Action.UI.DriverOnboarding.Idfy as DriverOnboarding
import qualified Domain.Action.UI.DriverOnboarding.Status as DriverOnboarding
import qualified Domain.Action.UI.DriverOnboarding.VehicleRegistrationCertificate as DriverOnboarding
import qualified Domain.Types.Person as DP
import Environment
import EulerHS.Prelude
import Servant
import Tools.Auth (TokenAuth)

type API =
  "driver" :> "register"
    :> ( "dl"
           :> ( TokenAuth
                  :> ReqBody '[JSON] DriverOnboarding.DriverDLReq
                  :> Post '[JSON] DriverOnboarding.DriverDLRes
                  :<|> "image"
                    :> TokenAuth
                    :> ReqBody '[JSON] DriverOnboarding.DriverDLImageReq
                    :> Post '[JSON] DriverOnboarding.DriverDLRes
              )
           :<|> "rc"
             :> ( TokenAuth
                    :> ReqBody '[JSON] DriverOnboarding.DriverRCReq
                    :> Post '[JSON] DriverOnboarding.DriverRCRes
                    :<|> "image"
                      :> TokenAuth
                      :> ReqBody '[JSON] DriverOnboarding.DriverRCImageReq
                      :> Post '[JSON] DriverOnboarding.DriverRCRes
                )
           :<|> "status"
             :> TokenAuth
             :> Get '[JSON] DriverOnboarding.StatusRes
       )
    :<|> "ext"
    :> "idfy"
    :> ( "drivingLicense"
           :> ReqBody '[JSON] DriverOnboarding.IdfyDLReq
           :> Post '[JSON] AckResponse
           :<|> "vehicleRegistrationCert"
             :> ReqBody '[JSON] DriverOnboarding.IdfyRCReq
             :> Post '[JSON] AckResponse
       )

handler :: FlowServer API
handler =
  ( ( verifyDL
        :<|> validateDLImage
    )
      :<|> ( verifyRC
               :<|> validateRCImage
           )
      :<|> statusImpl
  )
    :<|> ( idfyDrivingLicense
             :<|> idfyRCLicense
         )

validateDLImage :: Id DP.Person -> DriverOnboarding.DriverDLImageReq -> FlowHandler DriverOnboarding.DriverDLRes
validateDLImage personId = withFlowHandlerAPI . DriverOnboarding.validateDLImage personId

verifyDL :: Id DP.Person -> DriverOnboarding.DriverDLReq -> FlowHandler DriverOnboarding.DriverDLRes
verifyDL personId = withFlowHandlerAPI . DriverOnboarding.verifyDL personId

statusImpl :: Id DP.Person -> FlowHandler DriverOnboarding.StatusRes
statusImpl = withFlowHandlerAPI . DriverOnboarding.statusImpl

idfyDrivingLicense :: DriverOnboarding.IdfyDLReq -> FlowHandler AckResponse
idfyDrivingLicense = withFlowHandlerAPI . DriverOnboarding.idfyDrivingLicense

idfyRCLicense :: DriverOnboarding.IdfyRCReq -> FlowHandler AckResponse
idfyRCLicense = withFlowHandlerAPI . DriverOnboarding.idfyRCLicense

validateRCImage :: Id DP.Person -> DriverOnboarding.DriverRCImageReq -> FlowHandler DriverOnboarding.DriverRCRes
validateRCImage personId = withFlowHandlerAPI . DriverOnboarding.validateRCImage personId

verifyRC :: Id DP.Person -> DriverOnboarding.DriverRCReq -> FlowHandler DriverOnboarding.DriverRCRes
verifyRC personId = withFlowHandlerAPI . DriverOnboarding.verifyRC personId
