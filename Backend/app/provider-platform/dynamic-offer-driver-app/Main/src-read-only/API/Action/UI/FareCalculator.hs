{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.FareCalculator
  ( API,
    handler,
  )
where

import qualified API.Types.UI.FareCalculator
import qualified Control.Lens
import qualified Domain.Action.UI.FareCalculator
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.External.Maps.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API =
  ( TokenAuth :> "calculateFare" :> MandatoryQueryParam "distanceWeightage" Kernel.Prelude.Int
      :> MandatoryQueryParam
           "dropLatLon"
           Kernel.External.Maps.Types.LatLong
      :> MandatoryQueryParam "pickupLatLon" Kernel.External.Maps.Types.LatLong
      :> Get '[JSON] API.Types.UI.FareCalculator.FareResponse
  )

handler :: Environment.FlowServer API
handler = getCalculateFare

getCalculateFare ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Prelude.Int ->
    Kernel.External.Maps.Types.LatLong ->
    Kernel.External.Maps.Types.LatLong ->
    Environment.FlowHandler API.Types.UI.FareCalculator.FareResponse
  )
getCalculateFare a4 a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.FareCalculator.getCalculateFare (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a4) a3 a2 a1
