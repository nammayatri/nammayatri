{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.DriverAreaPreference where

import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id)
import qualified Kernel.External.Maps
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Servant
import Tools.Auth

data AreaPreferenceInfoRes = AreaPreferenceInfoRes {minCells :: Kernel.Prelude.Int, radiusArea :: Kernel.Prelude.Maybe RadiusAreaSelection, selectedGeohashAreas :: [SelectedGeohashArea]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data AreaPreferenceSelection
  = SelectGeohashCells [Kernel.Prelude.Text]
  | SelectRadiusArea RadiusAreaSelection
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data AreaPreferenceUpdateReq = AreaPreferenceUpdateReq {selection :: Kernel.Prelude.Maybe AreaPreferenceSelection}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data GeohashAreaItem = GeohashAreaItem {areaName :: Kernel.Prelude.Text, geohash :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RadiusAreaSelection = RadiusAreaSelection {center :: Kernel.External.Maps.LatLong, radiusMeters :: Kernel.Types.Common.Meters}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SelectedGeohashArea = SelectedGeohashArea {areaName :: Kernel.Prelude.Maybe Kernel.Prelude.Text, geohash :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
