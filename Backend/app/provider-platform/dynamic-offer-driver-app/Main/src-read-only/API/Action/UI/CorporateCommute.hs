{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.CorporateCommute
  ( API,
    handler,
  )
where

import qualified API.Types.UI.CorporateCommute
import qualified Control.Lens
import Data.Time.Calendar (Day)
import qualified Domain.Action.UI.CorporateCommute
import qualified Domain.Types.CorporateShift
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
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
  ( TokenAuth :> "driver" :> "corporate" :> "shifts" :> Get '[JSON] [API.Types.UI.CorporateCommute.CorporateShiftDriverResp]
      :<|> TokenAuth :> "driver" :> "corporate" :> "routes" :> Capture "shiftId" (Kernel.Types.Id.Id Domain.Types.CorporateShift.CorporateShift) :> Get '[JSON] [API.Types.UI.CorporateCommute.CorporateRouteDriverResp]
      :<|> TokenAuth :> "driver" :> "corporate" :> "roster" :> MandatoryQueryParam "date" Day :> Get '[JSON] [API.Types.UI.CorporateCommute.CorporateRosterDriverResp]
      :<|> TokenAuth :> "driver" :> "corporate" :> "ride" :> "accept" :> ReqBody '[JSON] API.Types.UI.CorporateCommute.AcceptCorporateRideReq :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
      :<|> TokenAuth :> "driver" :> "corporate" :> "earnings" :> Get '[JSON] API.Types.UI.CorporateCommute.CorporateEarningsSummary
  )

handler :: Environment.FlowServer API
handler =
  getDriverCorporateShifts
    :<|> getDriverCorporateRoutes
    :<|> getDriverCorporateRoster
    :<|> postDriverCorporateRideAccept
    :<|> getDriverCorporateEarnings

getDriverCorporateShifts ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Environment.FlowHandler [API.Types.UI.CorporateCommute.CorporateShiftDriverResp]
  )
getDriverCorporateShifts a1 = withFlowHandlerAPI $ Domain.Action.UI.CorporateCommute.getDriverCorporateShifts (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a1)

getDriverCorporateRoutes ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Types.Id.Id Domain.Types.CorporateShift.CorporateShift ->
    Environment.FlowHandler [API.Types.UI.CorporateCommute.CorporateRouteDriverResp]
  )
getDriverCorporateRoutes a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.CorporateCommute.getDriverCorporateRoutes (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

getDriverCorporateRoster ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Day ->
    Environment.FlowHandler [API.Types.UI.CorporateCommute.CorporateRosterDriverResp]
  )
getDriverCorporateRoster a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.CorporateCommute.getDriverCorporateRoster (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

postDriverCorporateRideAccept ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    API.Types.UI.CorporateCommute.AcceptCorporateRideReq ->
    Environment.FlowHandler Kernel.Types.APISuccess.APISuccess
  )
postDriverCorporateRideAccept a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.CorporateCommute.postDriverCorporateRideAccept (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

getDriverCorporateEarnings ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Environment.FlowHandler API.Types.UI.CorporateCommute.CorporateEarningsSummary
  )
getDriverCorporateEarnings a1 = withFlowHandlerAPI $ Domain.Action.UI.CorporateCommute.getDriverCorporateEarnings (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a1)
