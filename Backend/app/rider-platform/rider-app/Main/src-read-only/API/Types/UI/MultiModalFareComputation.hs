{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.MultiModalFareComputation where

import Data.OpenApi (ToSchema)
import qualified Domain.Types.MultiModalFareLegRules
import qualified Domain.Types.MultiModalNetwork
import EulerHS.Prelude hiding (id)
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import Servant
import Tools.Auth

data GetFareReq = GetFareReq
  { distance :: Kernel.Types.Common.Meters,
    media :: Domain.Types.MultiModalFareLegRules.MultiModalFareMediaType,
    networkId :: Kernel.Types.Id.Id Domain.Types.MultiModalNetwork.MultiModalNetwork,
    passengerType :: Domain.Types.MultiModalFareLegRules.MultiModalPassengerType
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data MultiModalFare = MultiModalFare {currency :: Kernel.Types.Common.Currency, fare :: Kernel.Types.Common.HighPrecMoney}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
