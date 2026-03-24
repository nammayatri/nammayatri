{-# OPTIONS_GHC -Wno-unused-imports #-}


module API.Types.UI.PreferredRoute where
import EulerHS.Prelude hiding (id)
import Servant
import Tools.Auth
import Data.OpenApi (ToSchema)
import qualified Domain.Types.LocationAddress
import qualified Kernel.Prelude
import qualified Data.Text



data PreferredRouteResp
    = PreferredRouteResp {fromLocationAddress :: Domain.Types.LocationAddress.LocationAddress,
                          fromLocationLat :: Kernel.Prelude.Double,
                          fromLocationLon :: Kernel.Prelude.Double,
                          id :: Data.Text.Text,
                          priority :: Kernel.Prelude.Int,
                          routeName :: Data.Text.Text,
                          toLocationAddress :: Domain.Types.LocationAddress.LocationAddress,
                          toLocationLat :: Kernel.Prelude.Double,
                          toLocationLon :: Kernel.Prelude.Double,
                          usageCount :: Kernel.Prelude.Int}
    deriving stock Generic
    deriving anyclass (ToJSON, FromJSON, ToSchema)



