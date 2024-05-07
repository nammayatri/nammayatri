{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.VehicleDetails where

import Data.OpenApi (ToSchema)
import qualified Data.Text
import EulerHS.Prelude hiding (id)
import Servant
import Tools.Auth

data VehicleDetailsReq = VehicleDetailsReq {make :: Data.Text.Text, model :: Data.Text.Text} deriving (Generic, ToJSON, FromJSON, ToSchema)

data VehicleModelsResp = VehicleModelsResp {makes :: [Data.Text.Text]} deriving (Generic, ToJSON, FromJSON, ToSchema)

data VehicleVariantsReq = VehicleVariantsReq {make :: Data.Text.Text} deriving (Generic, ToJSON, FromJSON, ToSchema)

data VehicleVariantsResp = VehicleVariantsResp {models :: [Data.Text.Text]} deriving (Generic, ToJSON, FromJSON, ToSchema)
