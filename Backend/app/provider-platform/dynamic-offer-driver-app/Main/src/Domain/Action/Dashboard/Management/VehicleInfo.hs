{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.Dashboard.Management.VehicleInfo
  ( getVehicleInfoList,
    putVehicleInfoUpdate,
  )
where

import qualified API.Types.ProviderPlatform.Management.VehicleInfo as Common
import Data.OpenApi (ToSchema)
-- import qualified Domain.Action.UI.VehicleInfo as UIVI
import qualified Domain.Types.Merchant
import qualified Domain.Types.VehicleInfo as DVI
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id as ID
import Servant
import Storage.Queries.VehicleInfo (updateVehicleInfo)
import Storage.Queries.VehicleInfoExtra (findAll)
import Tools.Auth

getVehicleInfoList ::
  ID.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Environment.Flow [Common.VehicleInfoAPIEntity]
getVehicleInfoList _merchantShortId _opCity = do
  vehicleInfoList <- findAll
  pure $ convertVehicleInfoToVehicleInfoAPIEntity <$> vehicleInfoList
  where
    convertVehicleInfoToVehicleInfoAPIEntity DVI.VehicleInfo {..} =
      Common.VehicleInfoAPIEntity
        { id = ID.cast id,
          rcId = ID.cast id,
          ..
        }

putVehicleInfoUpdate ::
  ID.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Common.UpdateVehicleInfoReq ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
putVehicleInfoUpdate _merchantShortId _opCity req = do
  mapM_ (\vi -> updateVehicleInfo vi.questionName vi.question vi.answer $ ID.cast vi.id) req.newInfo
  pure Kernel.Types.APISuccess.Success
