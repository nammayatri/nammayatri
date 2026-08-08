{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.FRFSDriverRatingInternal where

import Data.OpenApi (ToSchema)
import qualified Data.Text
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Servant
import Tools.Auth

data FRFSDriverRatingAggRes = FRFSDriverRatingAggRes
  { driverRating :: Kernel.Prelude.Maybe Kernel.Types.Common.Centesimal,
    driverRatingCount :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    fleetRating :: Kernel.Prelude.Maybe Kernel.Types.Common.Centesimal,
    fleetRatingCount :: Kernel.Prelude.Maybe Kernel.Prelude.Int
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FRFSDriverRatingReq = FRFSDriverRatingReq
  { bookingId :: Data.Text.Text,
    driverBadgeToken :: Data.Text.Text,
    driverRating :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    feedbackDetails :: Kernel.Prelude.Maybe Data.Text.Text,
    fleetNumber :: Kernel.Prelude.Maybe Data.Text.Text,
    fleetRating :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    gtfsId :: Kernel.Prelude.Maybe Data.Text.Text,
    merchantId :: Data.Text.Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
