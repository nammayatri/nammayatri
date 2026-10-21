{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.DemandHotspots where

import qualified Data.Aeson as Ae
import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id)
import qualified Kernel.External.Maps.Types
import qualified Kernel.Prelude
import Servant
import Tools.Auth

data GetDemandHotspotsResp = GetDemandHotspotsResp {createdAt :: Kernel.Prelude.UTCTime, expiryAt :: Kernel.Prelude.UTCTime, hotspotsDetails :: [HotspotsDetails], enableDemandHotspots :: Kernel.Prelude.Bool}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data HotspotCategoryCount = HotspotCategoryCount
  { tripOption :: Kernel.Prelude.Text,
    isScheduled :: Kernel.Prelude.Bool,
    frequency :: Kernel.Prelude.Int
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data HotspotsDetails = HotspotsDetails
  { frequency :: Kernel.Prelude.Int,
    location :: Kernel.External.Maps.Types.LatLong,
    categories :: [HotspotCategoryCount]
  }
  deriving stock (Generic)
  deriving anyclass (ToSchema)

instance ToJSON HotspotsDetails where
  toJSON details =
    Ae.object
      [ "frequency" Ae..= details.frequency,
        "location" Ae..= details.location,
        "categories" Ae..= details.categories
      ]

-- Older cached payloads only have frequency and location. Missing categories
-- means a member written before the product was stored.
instance FromJSON HotspotsDetails where
  parseJSON = Ae.withObject "HotspotsDetails" $ \obj ->
    HotspotsDetails
      <$> obj Ae..: "frequency"
      <*> obj Ae..: "location"
      <*> obj Ae..:? "categories" Ae..!= []
