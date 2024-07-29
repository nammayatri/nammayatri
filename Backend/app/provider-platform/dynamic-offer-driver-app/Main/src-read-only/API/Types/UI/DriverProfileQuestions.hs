{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.DriverProfileQuestions where

import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import Servant
import Tools.Auth

data DriverProfileQuesReq = DriverProfileQuesReq
  { aspirations :: Kernel.Prelude.Maybe [Kernel.Prelude.Text],
    expertAt :: Kernel.Prelude.Maybe [Kernel.Prelude.Text],
    hometown :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    pledges :: Kernel.Prelude.Maybe [Kernel.Prelude.Text],
    whyNY :: Kernel.Prelude.Maybe [Kernel.Prelude.Text]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DriverProfileQuesRes = DriverProfileQuesRes
  { aspirations :: [Kernel.Prelude.Text],
    expertAt :: [Kernel.Prelude.Text],
    hometown :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    pledges :: [Kernel.Prelude.Text],
    whyNY :: [Kernel.Prelude.Text]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
