{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.ProviderPlatform.Management.Driver where

import qualified Dashboard.ProviderPlatform.Driver
import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import Servant
import Servant.Client

data ClearDriverFeeReq = ClearDriverFeeReq
  { cgstPercentage :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    currency :: Kernel.Prelude.Maybe Kernel.Types.Common.Currency,
    feeType :: API.Types.ProviderPlatform.Management.Driver.DriverFeeType,
    platformFee :: Kernel.Types.Common.HighPrecMoney,
    sendManualLink :: Kernel.Prelude.Bool,
    serviceName :: Dashboard.ProviderPlatform.Driver.ServiceNames,
    sgstPercentage :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data DriverFeeType = PAYOUT_REGISTRATION | ONE_TIME_SECURITY_DEPOSIT deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema, Read, Kernel.Prelude.ToParamSchema)

type API = ("driver" :> PostDriverClearFee)

type PostDriverClearFee =
  ( "clearFee" :> MandatoryQueryParam "driverId" (Kernel.Types.Id.Id Dashboard.ProviderPlatform.Driver.Driver)
      :> ReqBody
           '[JSON]
           API.Types.ProviderPlatform.Management.Driver.ClearDriverFeeReq
      :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
  )

newtype DriverAPIs = DriverAPIs {postDriverClearFee :: Kernel.Types.Id.Id Dashboard.ProviderPlatform.Driver.Driver -> API.Types.ProviderPlatform.Management.Driver.ClearDriverFeeReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess}

mkDriverAPIs :: (Client EulerHS.Types.EulerClient API -> DriverAPIs)
mkDriverAPIs driverClient = (DriverAPIs {..})
  where
    postDriverClearFee = driverClient
