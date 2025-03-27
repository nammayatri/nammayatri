{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.UI.VehicleInfo
  ( getVehicleInfoList,
    putVehicleInfoUpdate,
  )
where

import qualified API.Types.UI.VehicleInfo as UIVI
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
import Storage.Queries.VehicleInfo (updateVehicleInfo)
import Storage.Queries.VehicleInfoExtra (findAll)
import Tools.Auth

getVehicleInfoList ::
  ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
    Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
  ) ->
  Environment.Flow [Domain.Types.VehicleInfo.VehicleInfo]
getVehicleInfoList _ = findAll

putVehicleInfoUpdate ::
  ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
    Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
  ) ->
  UIVI.UpdateVehicleInfoReq ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
putVehicleInfoUpdate _ req = do
  mapM_ (\vi -> updateVehicleInfo vi.questionName vi.question vi.answer vi.id) req.newInfo
  pure Kernel.Types.APISuccess.Success
