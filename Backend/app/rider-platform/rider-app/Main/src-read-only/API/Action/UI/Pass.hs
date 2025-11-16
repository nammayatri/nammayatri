{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.Pass
  ( API,
    handler,
  )
where

import qualified API.Types.UI.Pass
import qualified Control.Lens
import qualified Data.Text
import qualified Data.Time
import qualified Domain.Action.UI.Pass
import qualified Domain.Types.Merchant
import qualified Domain.Types.Pass
import qualified Domain.Types.Person
import qualified Domain.Types.PurchasedPass
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.External.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API =
  ( TokenAuth :> "multimodal" :> "pass" :> "availablePasses" :> QueryParam "language" Kernel.External.Types.Language
      :> Get
           '[JSON]
           [API.Types.UI.Pass.PassInfoAPIEntity]
      :<|> TokenAuth
      :> "multimodal"
      :> "pass"
      :> Capture
           "passId"
           (Kernel.Types.Id.Id Domain.Types.Pass.Pass)
      :> "select"
      :> QueryParam
           "startDate"
           Data.Time.Day
      :> MandatoryQueryParam
           "deviceId"
           Data.Text.Text
      :> Post
           '[JSON]
           API.Types.UI.Pass.PassSelectionAPIEntity
      :<|> TokenAuth
      :> "multimodal"
      :> "pass"
      :> "list"
      :> QueryParam
           "language"
           Kernel.External.Types.Language
      :> QueryParam
           "limit"
           Kernel.Prelude.Int
      :> QueryParam
           "offset"
           Kernel.Prelude.Int
      :> QueryParam
           "status"
           Domain.Types.PurchasedPass.StatusType
      :> MandatoryQueryParam
           "deviceId"
           Data.Text.Text
      :> Get
           '[JSON]
           [API.Types.UI.Pass.PurchasedPassAPIEntity]
      :<|> TokenAuth
      :> "multimodal"
      :> "pass"
      :> Capture
           "purchasedPassId"
           (Kernel.Types.Id.Id Domain.Types.PurchasedPass.PurchasedPass)
      :> "verify"
      :> ReqBody
           '[JSON]
           API.Types.UI.Pass.PassVerifyReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
      :<|> TokenAuth
      :> "multimodal"
      :> "pass"
      :> "switchDeviceId"
      :> ReqBody
           '[JSON]
           API.Types.UI.Pass.PassSwitchDeviceIdReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
      :<|> TokenAuth
      :> "multimodal"
      :> "pass"
      :> "transactions"
      :> QueryParam
           "limit"
           Kernel.Prelude.Int
      :> QueryParam
           "offset"
           Kernel.Prelude.Int
      :> Get
           '[JSON]
           [API.Types.UI.Pass.PurchasedPassTransactionAPIEntity]
  )

handler :: Environment.FlowServer API
handler = getMultimodalPassAvailablePasses :<|> postMultimodalPassSelect :<|> getMultimodalPassList :<|> postMultimodalPassVerify :<|> postMultimodalPassSwitchDeviceId :<|> getMultimodalPassTransactions

getMultimodalPassAvailablePasses ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Prelude.Maybe Kernel.External.Types.Language ->
    Environment.FlowHandler [API.Types.UI.Pass.PassInfoAPIEntity]
  )
getMultimodalPassAvailablePasses a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.Pass.getMultimodalPassAvailablePasses (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

postMultimodalPassSelect ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.Pass.Pass ->
    Kernel.Prelude.Maybe Data.Time.Day ->
    Data.Text.Text ->
    Environment.FlowHandler API.Types.UI.Pass.PassSelectionAPIEntity
  )
postMultimodalPassSelect a4 a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.Pass.postMultimodalPassSelect (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a4) a3 a2 a1

getMultimodalPassList ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Prelude.Maybe Kernel.External.Types.Language ->
    Kernel.Prelude.Maybe Kernel.Prelude.Int ->
    Kernel.Prelude.Maybe Kernel.Prelude.Int ->
    Kernel.Prelude.Maybe Domain.Types.PurchasedPass.StatusType ->
    Data.Text.Text ->
    Environment.FlowHandler [API.Types.UI.Pass.PurchasedPassAPIEntity]
  )
getMultimodalPassList a6 a5 a4 a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.Pass.getMultimodalPassList (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a6) a5 a4 a3 a2 a1

postMultimodalPassVerify ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.PurchasedPass.PurchasedPass ->
    API.Types.UI.Pass.PassVerifyReq ->
    Environment.FlowHandler Kernel.Types.APISuccess.APISuccess
  )
postMultimodalPassVerify a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.Pass.postMultimodalPassVerify (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a3) a2 a1

postMultimodalPassSwitchDeviceId ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    API.Types.UI.Pass.PassSwitchDeviceIdReq ->
    Environment.FlowHandler Kernel.Types.APISuccess.APISuccess
  )
postMultimodalPassSwitchDeviceId a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.Pass.postMultimodalPassSwitchDeviceId (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

getMultimodalPassTransactions ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Prelude.Maybe Kernel.Prelude.Int ->
    Kernel.Prelude.Maybe Kernel.Prelude.Int ->
    Environment.FlowHandler [API.Types.UI.Pass.PurchasedPassTransactionAPIEntity]
  )
getMultimodalPassTransactions a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.Pass.getMultimodalPassTransactions (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a3) a2 a1
