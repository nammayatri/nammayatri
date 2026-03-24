{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}

{-# LANGUAGE ApplicativeDo #-}
module Domain.Types.PlaceNameCache (module Domain.Types.PlaceNameCache, module ReExport) where
import Kernel.Prelude
import Data.Aeson
import Domain.Types.Extra.PlaceNameCache as ReExport
import qualified Domain.Types.Extra.PlaceNameCache
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH



data PlaceNameCache
    = PlaceNameCache {addressComponents :: [Domain.Types.Extra.PlaceNameCache.AddressResp],
                      formattedAddress :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                      geoHash :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                      id :: Kernel.Types.Id.Id Domain.Types.PlaceNameCache.PlaceNameCache,
                      lat :: Kernel.Prelude.Double,
                      lon :: Kernel.Prelude.Double,
                      placeId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                      plusCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                      createdAt :: Kernel.Prelude.UTCTime}
    deriving (Generic, Show, ToJSON, FromJSON, ToSchema)



