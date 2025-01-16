module Domain.Action.UI.VehicleDetails where

import qualified API.Types.UI.VehicleDetails
import Data.List (nub)
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Domain.Types.VehicleDetails
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.Common (fromMaybeM)
import qualified Storage.CachedQueries.VehicleDetails as QCVehicleDetails
import Tools.Error

getVehicleMakes ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Environment.Flow API.Types.UI.VehicleDetails.VehicleMakesResp
  )
getVehicleMakes (_, _, _) = do
  vehicleDetails <- QCVehicleDetails.findAllVehicleDetails
  let makes = map Domain.Types.VehicleDetails.make vehicleDetails
  let makesWithoutDuplicates = nub makes
  pure $ API.Types.UI.VehicleDetails.VehicleMakesResp makesWithoutDuplicates

postVehicleModels ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    API.Types.UI.VehicleDetails.VehicleModelsReq ->
    Environment.Flow API.Types.UI.VehicleDetails.VehicleModelsResp
  )
postVehicleModels (_, _, _) (API.Types.UI.VehicleDetails.VehicleModelsReq make) = do
  vehicleDetails <- QCVehicleDetails.findByMakeAndYear make Nothing
  let models = map Domain.Types.VehicleDetails.model vehicleDetails
  pure $ API.Types.UI.VehicleDetails.VehicleModelsResp models

postVehicleDetails ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    API.Types.UI.VehicleDetails.VehicleDetailsReq ->
    Environment.Flow Domain.Types.VehicleDetails.VehicleDetails
  )
postVehicleDetails (_, _, _) (API.Types.UI.VehicleDetails.VehicleDetailsReq make model) = QCVehicleDetails.findByMakeAndModelAndYear make model Nothing >>= fromMaybeM (InvalidRequest "vehicle detail not found")
