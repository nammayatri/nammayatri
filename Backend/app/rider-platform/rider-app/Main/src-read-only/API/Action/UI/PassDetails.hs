{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.PassDetails
  ( API,
    handler,
  )
where

import qualified API.Types.UI.PassDetails
import qualified Control.Lens
import qualified Domain.Action.UI.PassDetails
import qualified Domain.Types.Merchant
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
  ( TokenAuth :> "getOrganizations" :> Capture "passEnum" Kernel.Prelude.Text
      :> Get
           '[JSON]
           [API.Types.UI.PassDetails.GetOrganizationResp]
      :<|> TokenAuth
      :> "passDetails"
      :> Capture "passEnum" Kernel.Prelude.Text
      :> "update"
      :> ReqBody
           '[JSON]
           API.Types.UI.PassDetails.PassDetailsUpdateReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
      :<|> TokenAuth
      :> "passDetails"
      :> Capture
           "passEnum"
           Kernel.Prelude.Text
      :> "data"
      :> Get
           '[JSON]
           API.Types.UI.PassDetails.PassDetailsDataResp
      :<|> TokenAuth
      :> "passDetails"
      :> Capture
           "passEnum"
           Kernel.Prelude.Text
      :> "verificationStatus"
      :> Get
           '[JSON]
           API.Types.UI.PassDetails.PassStatusResp
  )

handler :: Environment.FlowServer API
handler = getGetOrganizations :<|> postPassDetailsUpdate :<|> getPassDetailsData :<|> getPassDetailsVerificationStatus

getGetOrganizations ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Prelude.Text ->
    Environment.FlowHandler [API.Types.UI.PassDetails.GetOrganizationResp]
  )
getGetOrganizations a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.PassDetails.getGetOrganizations (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

postPassDetailsUpdate ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Prelude.Text ->
    API.Types.UI.PassDetails.PassDetailsUpdateReq ->
    Environment.FlowHandler Kernel.Types.APISuccess.APISuccess
  )
postPassDetailsUpdate a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.PassDetails.postPassDetailsUpdate (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a3) a2 a1

getPassDetailsData ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Prelude.Text ->
    Environment.FlowHandler API.Types.UI.PassDetails.PassDetailsDataResp
  )
getPassDetailsData a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.PassDetails.getPassDetailsData (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

getPassDetailsVerificationStatus ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Prelude.Text ->
    Environment.FlowHandler API.Types.UI.PassDetails.PassStatusResp
  )
getPassDetailsVerificationStatus a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.PassDetails.getPassDetailsVerificationStatus (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1
