{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.PassService
  ( API,
    handler,
  )
where

import qualified API.Types.UI.PassService
import qualified Control.Lens
import qualified Domain.Action.UI.PassService as Domain.Action.UI.PassService
import qualified Domain.Types.Merchant
import qualified Domain.Types.Pass
import qualified Domain.Types.PassCategory
import qualified Domain.Types.Person
import qualified Domain.Types.PurchasedPass
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API =
  ( TokenAuth :> "passes" :> Get ('[JSON]) [API.Types.UI.PassService.PassCategoryAPIEntity] :<|> TokenAuth
      :> Capture
           "categoryId"
           (Kernel.Types.Id.Id Domain.Types.PassCategory.PassCategory)
      :> "details"
      :> Get ('[JSON]) [API.Types.UI.PassService.PassAPIEntity]
      :<|> TokenAuth
      :> Capture
           "passId"
           (Kernel.Types.Id.Id Domain.Types.Pass.Pass)
      :> "select"
      :> Post
           ('[JSON])
           API.Types.UI.PassService.PassSelectionAPIEntity
      :<|> TokenAuth
      :> "purchased"
      :> Capture
           "purchasedPassId"
           (Kernel.Types.Id.Id Domain.Types.PurchasedPass.PurchasedPass)
      :> "detail"
      :> Get
           ('[JSON])
           API.Types.UI.PassService.PurchasedPassAPIEntity
      :<|> TokenAuth
      :> "purchased"
      :> "list"
      :> MandatoryQueryParam
           "limit"
           Kernel.Prelude.Int
      :> MandatoryQueryParam
           "offset"
           Kernel.Prelude.Int
      :> Get
           ('[JSON])
           [API.Types.UI.PassService.PurchasedPassAPIEntity]
  )

handler :: Environment.FlowServer API
handler = getPasses :<|> getDetails :<|> postSelect :<|> getPurchasedDetail :<|> getPurchasedList

getPasses :: ((Kernel.Types.Id.Id Domain.Types.Person.Person, Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Environment.FlowHandler [API.Types.UI.PassService.PassCategoryAPIEntity])
getPasses a1 = withFlowHandlerAPI $ Domain.Action.UI.PassService.getPasses (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a1)

getDetails ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.PassCategory.PassCategory ->
    Environment.FlowHandler [API.Types.UI.PassService.PassAPIEntity]
  )
getDetails a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.PassService.getDetails (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

postSelect ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.Pass.Pass ->
    Environment.FlowHandler API.Types.UI.PassService.PassSelectionAPIEntity
  )
postSelect a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.PassService.postSelect (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

getPurchasedDetail ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.PurchasedPass.PurchasedPass ->
    Environment.FlowHandler API.Types.UI.PassService.PurchasedPassAPIEntity
  )
getPurchasedDetail a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.PassService.getPurchasedDetail (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

getPurchasedList ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Prelude.Int ->
    Kernel.Prelude.Int ->
    Environment.FlowHandler [API.Types.UI.PassService.PurchasedPassAPIEntity]
  )
getPurchasedList a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.PassService.getPurchasedList (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a3) a2 a1
