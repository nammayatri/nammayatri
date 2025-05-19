{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.Merchant where

import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import Servant
import Tools.Auth

data CityConfigs = CityConfigs {localAmbulanceNumbers :: [Kernel.Prelude.Text], localPoliceNumbers :: [Kernel.Prelude.Text], safetyTeamNumbers :: [Kernel.Prelude.Text]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
