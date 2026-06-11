{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.CityInfo where

import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import Servant
import Tools.Auth

data StdCodeResp = StdCodeResp {cityName :: Kernel.Prelude.Text, stdCode :: Kernel.Prelude.Text}
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
