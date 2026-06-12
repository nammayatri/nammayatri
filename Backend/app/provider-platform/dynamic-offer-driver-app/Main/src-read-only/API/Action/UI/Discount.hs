{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.Discount
  ( API,
    handler,
  )
where

import qualified API.Types.UI.Discount
import qualified Control.Lens
import qualified Domain.Action.UI.Discount
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Domain.Types.Plan
import qualified Domain.Types.VehicleCategory
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.External.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API =
  ( TokenAuth :> "discount" :> "list" :> QueryParam "language" Kernel.External.Types.Language :> QueryParam "discountType" Kernel.Prelude.Text
      :> QueryParam
           "planId"
           (Kernel.Types.Id.Id Domain.Types.Plan.Plan)
      :> QueryParam
           "vehicleCategory"
           Domain.Types.VehicleCategory.VehicleCategory
      :> QueryParam
           "paymentMode"
           Domain.Types.Plan.PaymentMode
      :> Get
           '[JSON]
           API.Types.UI.Discount.GetDiscountsRes
  )

handler :: Environment.FlowServer API
handler = getDiscountList

getDiscountList ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Prelude.Maybe Kernel.External.Types.Language ->
    Kernel.Prelude.Maybe Kernel.Prelude.Text ->
    Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Plan.Plan) ->
    Kernel.Prelude.Maybe Domain.Types.VehicleCategory.VehicleCategory ->
    Kernel.Prelude.Maybe Domain.Types.Plan.PaymentMode ->
    Environment.FlowHandler API.Types.UI.Discount.GetDiscountsRes
  )
getDiscountList a6 a5 a4 a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.Discount.getDiscountList (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a6) a5 a4 a3 a2 a1
