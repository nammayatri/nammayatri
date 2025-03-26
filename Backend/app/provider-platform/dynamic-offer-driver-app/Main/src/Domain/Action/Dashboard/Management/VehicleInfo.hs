{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.Dashboard.Management.VehicleInfo
  ( getVehicleInfoList,
    putVehicleInfoUpdate,
  )
where

import qualified API.Types.ProviderPlatform.Management.VehicleInfo
import Data.OpenApi (ToSchema)
import qualified Domain.Types.Merchant
import qualified Domain.Types.VehicleInfo
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Servant
import Tools.Auth

getVehicleInfoList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.Flow [Domain.Types.VehicleInfo.VehicleInfo])
getVehicleInfoList _merchantShortId _opCity = do error "Logic yet to be decided"

putVehicleInfoUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Management.VehicleInfo.UpdateVehicleInfoReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
putVehicleInfoUpdate _merchantShortId _opCity req = do error "Logic yet to be decided" req
