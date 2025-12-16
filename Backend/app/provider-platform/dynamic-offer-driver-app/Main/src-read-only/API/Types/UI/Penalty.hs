{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.Penalty where

import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id)
import qualified Kernel.External.Maps.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Servant
import Tools.Auth

data PenaltyCheckReq = PenaltyCheckReq {rideId :: Kernel.Prelude.Text, point :: Kernel.External.Maps.Types.LatLong}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PenaltyCheckRes = PenaltyCheckRes
  { isCancellationPenaltyApplicable :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    cancellationPenaltyAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    cancellationValidity :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
