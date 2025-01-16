{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.DemandHotspots where

import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id)
import qualified Kernel.External.Maps.Types
import qualified Kernel.Prelude
import Servant
import Tools.Auth

data GetDemandHotspotsResp = GetDemandHotspotsResp {createdAt :: Kernel.Prelude.UTCTime, expiryAt :: Kernel.Prelude.UTCTime, hotspotsDetails :: [HotspotsDetails]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data HotspotsDetails = HotspotsDetails {frequency :: Kernel.Prelude.Int, location :: Kernel.External.Maps.Types.LatLong}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
