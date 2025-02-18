{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.PlaceNameCache where

import Data.Aeson
import qualified Domain.Action.UI.PlaceNameCache
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data PlaceNameCache = PlaceNameCache
  { addressComponents :: [Domain.Action.UI.PlaceNameCache.AddressResp],
    createdAt :: Kernel.Prelude.UTCTime,
    formattedAddress :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    geoHash :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Domain.Types.PlaceNameCache.PlaceNameCache,
    lat :: Kernel.Prelude.Double,
    lon :: Kernel.Prelude.Double,
    placeId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    plusCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
