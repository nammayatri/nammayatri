{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.DriverConduct where

import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import Servant
import Tools.Auth

data CurrentConsequence = CurrentConsequence
  { appliedAt :: Kernel.Prelude.UTCTime,
    consequenceType :: Kernel.Prelude.Text,
    description :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    imageUrl :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    okButtonText :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    programme :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    title :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    validTill :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DriverConductCurrentRes = DriverConductCurrentRes {current :: Kernel.Prelude.Maybe CurrentConsequence}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
