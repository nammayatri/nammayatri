{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.Student
  ( API,
    handler,
  )
where

import qualified API.Types.UI.Student
import qualified Control.Lens
import qualified Domain.Action.UI.Student
import qualified Domain.Types.Merchant
import qualified Domain.Types.Pass
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API =
  ( TokenAuth :> "getOrganizations" :> Get ('[JSON]) [API.Types.UI.Student.GetOrganizationResp] :<|> TokenAuth :> "student" :> "update"
      :> ReqBody
           ('[JSON])
           API.Types.UI.Student.StudentUpdateReq
      :> Post ('[JSON]) Kernel.Types.APISuccess.APISuccess
      :<|> TokenAuth
      :> "student"
      :> "data"
      :> Get
           ('[JSON])
           API.Types.UI.Student.StudentDataResp
      :<|> TokenAuth
      :> "student"
      :> "verificationStatus"
      :> Get
           ('[JSON])
           API.Types.UI.Student.StudentStatusResp
      :<|> TokenAuth
      :> "student"
      :> Capture
           "passId"
           (Kernel.Types.Id.Id Domain.Types.Pass.Pass)
      :> "activate"
      :> ReqBody
           ('[JSON])
           API.Types.UI.Student.StudentActivateReq
      :> Post
           ('[JSON])
           Kernel.Types.APISuccess.APISuccess
  )

handler :: Environment.FlowServer API
handler = getGetOrganizations :<|> postStudentUpdate :<|> getStudentData :<|> getStudentVerificationStatus :<|> postStudentActivate

getGetOrganizations :: ((Kernel.Types.Id.Id Domain.Types.Person.Person, Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Environment.FlowHandler [API.Types.UI.Student.GetOrganizationResp])
getGetOrganizations a1 = withFlowHandlerAPI $ Domain.Action.UI.Student.getGetOrganizations (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a1)

postStudentUpdate ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    API.Types.UI.Student.StudentUpdateReq ->
    Environment.FlowHandler Kernel.Types.APISuccess.APISuccess
  )
postStudentUpdate a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.Student.postStudentUpdate (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

getStudentData :: ((Kernel.Types.Id.Id Domain.Types.Person.Person, Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Environment.FlowHandler API.Types.UI.Student.StudentDataResp)
getStudentData a1 = withFlowHandlerAPI $ Domain.Action.UI.Student.getStudentData (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a1)

getStudentVerificationStatus :: ((Kernel.Types.Id.Id Domain.Types.Person.Person, Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Environment.FlowHandler API.Types.UI.Student.StudentStatusResp)
getStudentVerificationStatus a1 = withFlowHandlerAPI $ Domain.Action.UI.Student.getStudentVerificationStatus (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a1)

postStudentActivate ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.Pass.Pass ->
    API.Types.UI.Student.StudentActivateReq ->
    Environment.FlowHandler Kernel.Types.APISuccess.APISuccess
  )
postStudentActivate a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.Student.postStudentActivate (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a3) a2 a1
