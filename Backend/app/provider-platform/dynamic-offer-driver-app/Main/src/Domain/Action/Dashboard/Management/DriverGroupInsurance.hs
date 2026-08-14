module Domain.Action.Dashboard.Management.DriverGroupInsurance
  ( postDriverGroupInsuranceSecondBotCheck,
    postDriverGroupInsuranceEnable,
  )
where

import qualified Dashboard.Common
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id

postDriverGroupInsuranceSecondBotCheck :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.DriverGroupInsurance -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postDriverGroupInsuranceSecondBotCheck _merchantShortId _opCity insuranceId = do error "Logic yet to be decided" insuranceId

postDriverGroupInsuranceEnable :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.DriverGroupInsurance -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postDriverGroupInsuranceEnable _merchantShortId _opCity insuranceId = do error "Logic yet to be decided" insuranceId
