{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.DriverAreaPreference
  ( API,
    handler,
  )
where

import qualified API.Types.UI.DriverAreaPreference
import qualified Control.Lens
import qualified Domain.Action.UI.DriverAreaPreference
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API =
  ( TokenAuth :> "driver" :> "areaPreference" :> "getInfo"
      :> Get
           ('[JSON])
           API.Types.UI.DriverAreaPreference.AreaPreferenceInfoRes
      :<|> TokenAuth
      :> "driver"
      :> "areaPreference"
      :> "updateInfo"
      :> ReqBody
           ('[JSON])
           API.Types.UI.DriverAreaPreference.AreaPreferenceUpdateReq
      :> Post
           ('[JSON])
           API.Types.UI.DriverAreaPreference.AreaPreferenceInfoRes
      :<|> TokenAuth
      :> "driver"
      :> "areaPreference"
      :> "list"
      :> Get
           ('[JSON])
           [API.Types.UI.DriverAreaPreference.GeohashAreaItem]
  )

handler :: Environment.FlowServer API
handler = getDriverAreaPreferenceGetInfo :<|> postDriverAreaPreferenceUpdateInfo :<|> getDriverAreaPreferenceList

getDriverAreaPreferenceGetInfo ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Environment.FlowHandler API.Types.UI.DriverAreaPreference.AreaPreferenceInfoRes
  )
getDriverAreaPreferenceGetInfo a1 = withFlowHandlerAPI $ Domain.Action.UI.DriverAreaPreference.getDriverAreaPreferenceGetInfo (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a1)

postDriverAreaPreferenceUpdateInfo ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    API.Types.UI.DriverAreaPreference.AreaPreferenceUpdateReq ->
    Environment.FlowHandler API.Types.UI.DriverAreaPreference.AreaPreferenceInfoRes
  )
postDriverAreaPreferenceUpdateInfo a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.DriverAreaPreference.postDriverAreaPreferenceUpdateInfo (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

getDriverAreaPreferenceList ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Environment.FlowHandler [API.Types.UI.DriverAreaPreference.GeohashAreaItem]
  )
getDriverAreaPreferenceList a1 = withFlowHandlerAPI $ Domain.Action.UI.DriverAreaPreference.getDriverAreaPreferenceList (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a1)
