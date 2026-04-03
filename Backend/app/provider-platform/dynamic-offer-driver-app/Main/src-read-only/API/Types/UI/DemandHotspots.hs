{-# OPTIONS_GHC -Wno-unused-imports #-}


module API.Types.UI.DemandHotspots where
import EulerHS.Prelude hiding (id)
import Servant
import Tools.Auth
import Data.OpenApi (ToSchema)
import qualified Kernel.Prelude
import qualified Kernel.External.Maps.Types



data GetDemandHotspotsResp
    = GetDemandHotspotsResp {createdAt :: Kernel.Prelude.UTCTime, expiryAt :: Kernel.Prelude.UTCTime, hotspotsDetails :: [HotspotsDetails]}
    deriving stock Generic
    deriving anyclass (ToJSON, FromJSON, ToSchema)
data HotspotsDetails
    = HotspotsDetails {frequency :: Kernel.Prelude.Int, location :: Kernel.External.Maps.Types.LatLong}
    deriving stock Generic
    deriving anyclass (ToJSON, FromJSON, ToSchema)



