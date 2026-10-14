{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.RideEvidence where

import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import Servant
import Tools.Auth

data RideEvidenceItem = RideEvidenceItem {createdAt :: Kernel.Prelude.UTCTime, description :: Kernel.Prelude.Maybe Kernel.Prelude.Text, id :: Kernel.Prelude.Text, rideStatus :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
