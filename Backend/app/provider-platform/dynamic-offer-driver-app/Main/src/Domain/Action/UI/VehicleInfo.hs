{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.UI.VehicleInfo
  ( getVehicleInfoList,
    putVehicleInfoUpdate,
  )
where

import qualified API.Types.UI.VehicleInfo
import Data.OpenApi (ToSchema)
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Domain.Types.VehicleInfo
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Id
import Servant
import Tools.Auth

getVehicleInfoList ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Environment.Flow [Domain.Types.VehicleInfo.VehicleInfo]
  )
getVehicleInfoList = do error "Logic yet to be decided"

putVehicleInfoUpdate ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    API.Types.UI.VehicleInfo.UpdateVehicleInfoReq ->
    Environment.Flow Kernel.Types.APISuccess.APISuccess
  )
putVehicleInfoUpdate = do error "Logic yet to be decided"
