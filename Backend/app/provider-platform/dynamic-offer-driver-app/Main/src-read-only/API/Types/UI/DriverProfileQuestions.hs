{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.DriverProfileQuestions where

import Data.OpenApi (ToSchema)
import qualified Data.Time.Calendar
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import Servant
import Tools.Auth

data DriverProfileQuesReq = DriverProfileQuesReq
  { anniversary :: Kernel.Prelude.Maybe Data.Time.Calendar.Day,
    aspirations :: [Kernel.Prelude.Text],
    birthday :: Kernel.Prelude.Maybe Data.Time.Calendar.Day,
    drivingSince :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    hometown :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    imageIds :: [Kernel.Prelude.Text],
    pledges :: [Kernel.Prelude.Text],
    vehicleTags :: [Kernel.Prelude.Text]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DriverProfileQuesRes = DriverProfileQuesRes
  { anniversary :: Kernel.Prelude.Maybe Data.Time.Calendar.Day,
    aspirations :: [Kernel.Prelude.Text],
    birthday :: Kernel.Prelude.Maybe Data.Time.Calendar.Day,
    drivingSince :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    hometown :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    otherImageIds :: [Kernel.Prelude.Text],
    otherImages :: [Kernel.Prelude.Text],
    pledges :: [Kernel.Prelude.Text],
    profileImage :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    vehicleImage :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    vehicleTags :: [Kernel.Prelude.Text]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
