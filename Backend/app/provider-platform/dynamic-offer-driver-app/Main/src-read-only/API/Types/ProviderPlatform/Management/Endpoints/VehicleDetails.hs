{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.ProviderPlatform.Management.Endpoints.VehicleDetails where

import qualified Data.Aeson
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import Kernel.Types.Common
import Servant
import Servant.Client

data VehicleMakeModelsItem = VehicleMakeModelsItem {make :: Kernel.Prelude.Text, models :: [Kernel.Prelude.Text]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type API = ("vehicleDetails" :> GetVehicleDetailsVehicleModels)

type GetVehicleDetailsVehicleModels = ("vehicleModels" :> Get ('[JSON]) [VehicleMakeModelsItem])

newtype VehicleDetailsAPIs = VehicleDetailsAPIs {getVehicleDetailsVehicleModels :: (EulerHS.Types.EulerClient [VehicleMakeModelsItem])}

mkVehicleDetailsAPIs :: (Client EulerHS.Types.EulerClient API -> VehicleDetailsAPIs)
mkVehicleDetailsAPIs vehicleDetailsClient = (VehicleDetailsAPIs {..})
  where
    getVehicleDetailsVehicleModels = vehicleDetailsClient

data VehicleDetailsUserActionType
  = GET_VEHICLE_DETAILS_VEHICLE_MODELS
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToSchema)

instance ToJSON VehicleDetailsUserActionType where
  toJSON (GET_VEHICLE_DETAILS_VEHICLE_MODELS) = Data.Aeson.String "GET_VEHICLE_DETAILS_VEHICLE_MODELS"

instance FromJSON VehicleDetailsUserActionType where
  parseJSON (Data.Aeson.String "GET_VEHICLE_DETAILS_VEHICLE_MODELS") = pure GET_VEHICLE_DETAILS_VEHICLE_MODELS
  parseJSON _ = fail "GET_VEHICLE_DETAILS_VEHICLE_MODELS expected"

$(Data.Singletons.TH.genSingletons [(''VehicleDetailsUserActionType)])
