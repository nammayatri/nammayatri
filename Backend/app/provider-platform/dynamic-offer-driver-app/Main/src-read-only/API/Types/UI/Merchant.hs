{-# OPTIONS_GHC -Wno-unused-imports #-}


module API.Types.UI.Merchant where
import EulerHS.Prelude hiding (id)
import Servant
import Tools.Auth
import Data.OpenApi (ToSchema)
import qualified Kernel.Prelude



data CityConfigs
    = CityConfigs {localAmbulanceNumbers :: [Kernel.Prelude.Text], localPoliceNumbers :: [Kernel.Prelude.Text], safetyTeamNumbers :: [Kernel.Prelude.Text]}
    deriving stock Generic
    deriving anyclass (ToJSON, FromJSON, ToSchema)



