{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Action.UI.VehicleDetails where

import qualified API.Types.UI.VehicleDetails
import Control.Applicative (Applicative (pure))
import Data.List (nub)
import Data.Maybe (fromMaybe)
import Data.OpenApi (ToSchema)
import qualified Domain.Types.Merchant
import qualified Domain.Types.Merchant.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Domain.Types.VehicleDetails
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.Common (fromMaybeM)
import Servant
import qualified Storage.CachedQueries.VehicleDetails as QCVehicleDetails
import Tools.Auth
import Tools.Error

getVehicleMakes ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.Merchant.MerchantOperatingCity.MerchantOperatingCity
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
      Kernel.Types.Id.Id Domain.Types.Merchant.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    API.Types.UI.VehicleDetails.VehicleModelsReq ->
    Environment.Flow API.Types.UI.VehicleDetails.VehicleModelsResp
  )
postVehicleModels (_, _, _) (API.Types.UI.VehicleDetails.VehicleModelsReq make) = do
  vehicleDetails <- QCVehicleDetails.findByMake make
  let models = map Domain.Types.VehicleDetails.model vehicleDetails
  pure $ API.Types.UI.VehicleDetails.VehicleModelsResp models

postVehicleDetails ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.Merchant.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    API.Types.UI.VehicleDetails.VehicleDetailsReq ->
    Environment.Flow Domain.Types.VehicleDetails.VehicleDetails
  )
postVehicleDetails (_, _, _) (API.Types.UI.VehicleDetails.VehicleDetailsReq make model) = QCVehicleDetails.findByMakeAndModel make model >>= fromMaybeM (InvalidRequest "vehicle detail not found")
