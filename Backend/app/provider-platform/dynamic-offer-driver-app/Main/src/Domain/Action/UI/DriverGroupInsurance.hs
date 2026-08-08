module Domain.Action.UI.DriverGroupInsurance
  ( getDriverGroupInsuranceGet,
    postDriverGroupInsuranceUpsert,
  )
where

import qualified API.Types.UI.DriverGroupInsurance
import qualified Domain.Types.DriverGroupInsurance
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.Id

getDriverGroupInsuranceGet ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Prelude.Maybe (Domain.Types.DriverGroupInsurance.DriverGroupInsuranceType) ->
    Environment.Flow API.Types.UI.DriverGroupInsurance.DriverGroupInsuranceRes
  )
getDriverGroupInsuranceGet = do error "Logic yet to be decided"

postDriverGroupInsuranceUpsert ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    API.Types.UI.DriverGroupInsurance.DriverGroupInsuranceUpsertReq ->
    Environment.Flow API.Types.UI.DriverGroupInsurance.DriverGroupInsuranceRes
  )
postDriverGroupInsuranceUpsert = do error "Logic yet to be decided"
