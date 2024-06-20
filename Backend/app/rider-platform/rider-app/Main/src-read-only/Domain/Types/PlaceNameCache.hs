{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.PlaceNameCache (module Domain.Types.PlaceNameCache, module ReExport) where

import Data.Aeson
import Domain.Types.Extra.PlaceNameCache as ReExport
import qualified Domain.Types.Extra.PlaceNameCache
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data PlaceNameCache = PlaceNameCache
  { id :: Kernel.Types.Id.Id Domain.Types.PlaceNameCache.PlaceNameCache,
    formattedAddress :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    plusCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    lat :: Kernel.Prelude.Double,
    lon :: Kernel.Prelude.Double,
    placeId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    addressComponents :: [Domain.Types.Extra.PlaceNameCache.AddressResp],
    geoHash :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
