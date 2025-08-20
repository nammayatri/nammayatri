{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.RiderMobileNumber
  ( API,
    handler,
  )
where

import qualified Control.Lens
import qualified Domain.Action.UI.RiderMobileNumber as Domain.Action.UI.RiderMobileNumber
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import qualified SharedLogic.CallBAPInternal
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API = (TokenAuth :> "riderMobileNumber" :> Capture "rideId" Kernel.Prelude.Text :> Get '[JSON] SharedLogic.CallBAPInternal.RiderMobileAPIEntity)

handler :: Environment.FlowServer API
handler = getRiderMobileNumber

getRiderMobileNumber ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Prelude.Text ->
    Environment.FlowHandler SharedLogic.CallBAPInternal.RiderMobileAPIEntity
  )
getRiderMobileNumber a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.RiderMobileNumber.getRiderMobileNumber (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1
