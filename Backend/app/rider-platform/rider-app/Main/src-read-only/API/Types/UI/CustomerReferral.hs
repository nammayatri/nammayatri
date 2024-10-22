{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.CustomerReferral where

import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id)
import qualified Kernel.External.Maps.Types
import qualified Kernel.Prelude
import qualified Kernel.Utils.Common
import Servant
import Tools.Auth

data ApplyCodeReq = ApplyCodeReq
  { androidId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    code :: Kernel.Prelude.Text,
    deviceId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    gps :: Kernel.Prelude.Maybe Kernel.External.Maps.Types.LatLong
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ReferredCustomers = ReferredCustomers {count :: Kernel.Prelude.Int}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ReferrerInfo = ReferrerInfo
  { applicableServiceTiers :: Kernel.Prelude.Maybe [Kernel.Prelude.Text],
    firstName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    lastName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    middleName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    rating :: Kernel.Prelude.Maybe Kernel.Utils.Common.Centesimal,
    referrerImageUri :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    registeredAt :: Kernel.Prelude.UTCTime,
    totalRides :: Kernel.Prelude.Int,
    vehicleNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    vehicleVariant :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
