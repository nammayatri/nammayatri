{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.SpecialLocationWarrior where

import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Lib.Queries.SpecialLocation
import qualified Lib.Types.SpecialLocation
import Servant
import Tools.Auth

data SpecialLocWarriorInfoReq = SpecialLocWarriorInfoReq
  { isSpecialLocWarrior :: Kernel.Prelude.Bool,
    preferredPrimarySpecialLocId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Lib.Types.SpecialLocation.SpecialLocation),
    preferredSecondarySpecialLocIds :: [Kernel.Types.Id.Id Lib.Types.SpecialLocation.SpecialLocation]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SpecialLocWarriorInfoRes = SpecialLocWarriorInfoRes
  { isSpecialLocWarrior :: Kernel.Prelude.Bool,
    preferredPrimarySpecialLoc :: Kernel.Prelude.Maybe Lib.Queries.SpecialLocation.SpecialLocationWarrior,
    preferredSecondarySpecialLocIds :: [Kernel.Types.Id.Id Lib.Types.SpecialLocation.SpecialLocation]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
