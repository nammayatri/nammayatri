{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.DriverGroupInsurance
  ( API,
    handler,
  )
where

import qualified API.Types.UI.DriverGroupInsurance
import qualified Control.Lens
import qualified Domain.Action.UI.DriverGroupInsurance
import qualified Domain.Types.DriverGroupInsurance
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
  ( TokenAuth :> "driverGroupInsurance" :> "get" :> QueryParam "insuranceType" Domain.Types.DriverGroupInsurance.DriverGroupInsuranceType
      :> Get
           ('[JSON])
           API.Types.UI.DriverGroupInsurance.DriverGroupInsuranceRes
      :<|> TokenAuth
      :> "driverGroupInsurance"
      :> "upsert"
      :> ReqBody
           ('[JSON])
           API.Types.UI.DriverGroupInsurance.DriverGroupInsuranceUpsertReq
      :> Post
           ('[JSON])
           API.Types.UI.DriverGroupInsurance.DriverGroupInsuranceRes
  )

handler :: Environment.FlowServer API
handler = getDriverGroupInsuranceGet :<|> postDriverGroupInsuranceUpsert

getDriverGroupInsuranceGet ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Prelude.Maybe (Domain.Types.DriverGroupInsurance.DriverGroupInsuranceType) ->
    Environment.FlowHandler API.Types.UI.DriverGroupInsurance.DriverGroupInsuranceRes
  )
getDriverGroupInsuranceGet a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.DriverGroupInsurance.getDriverGroupInsuranceGet (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

postDriverGroupInsuranceUpsert ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    API.Types.UI.DriverGroupInsurance.DriverGroupInsuranceUpsertReq ->
    Environment.FlowHandler API.Types.UI.DriverGroupInsurance.DriverGroupInsuranceRes
  )
postDriverGroupInsuranceUpsert a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.DriverGroupInsurance.postDriverGroupInsuranceUpsert (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1
