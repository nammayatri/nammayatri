module API.UI.DriverInsurance
  ( API,
    handler,
  )
where

import qualified Domain.Action.UI.Insurance as Insurance
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import qualified SharedLogic.CallBAPInternal
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API =
  TokenAuth
    :> "driver"
    :> "insurance"
    :> Get '[JSON] SharedLogic.CallBAPInternal.InsuranceAPIEntity

handler :: Environment.FlowServer API
handler = getDriverInsurance

getDriverInsurance ::
  ( Kernel.Types.Id.Id Domain.Types.Person.Person,
    Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
  ) ->
  Environment.FlowHandler SharedLogic.CallBAPInternal.InsuranceAPIEntity
getDriverInsurance auth = withFlowHandlerAPI $ Insurance.getDriverInsurance auth
