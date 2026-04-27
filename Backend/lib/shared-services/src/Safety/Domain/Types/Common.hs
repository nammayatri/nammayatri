{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Safety.Domain.Types.Common
  ( RideShareOptions (..),
    Person,
    Merchant,
    Ride,
    MerchantOperatingCity,
    Booking,
    ExternalSOSFlow (..),
    ExternalSOSTriggerSource (..),
    ExternalSOSConfig (..),
    flowToSOSService,
  )
where

import Data.Aeson
import Kernel.Beam.Lib.UtilsTH (mkBeamInstancesForEnumAndList)
import qualified Kernel.External.SOS.Types as SOS
import Kernel.Prelude
import Kernel.Types.Common (Seconds)
import Kernel.Utils.TH (mkFromHttpInstanceForEnum)

data RideShareOptions
  = ALWAYS_SHARE
  | SHARE_WITH_TIME_CONSTRAINTS
  | NEVER_SHARE
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data Person

data Merchant

data Ride

data MerchantOperatingCity

data Booking

-- | External SOS flow — mirrors rider's `Domain.Types.RiderConfig.ExternalSOSFlow`.
-- Lives here so shared-services' generated `SosDetailsRes` (which references
-- `externalSOSConfig`) can compile without pulling rider-app types.
data ExternalSOSFlow = ERSS | GJ112 | Trinity
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

-- | External SOS trigger source — mirrors rider's
-- `Domain.Types.RiderConfig.ExternalSOSTriggerSource`.
data ExternalSOSTriggerSource = FRONTEND | DASHBOARD
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

-- | External SOS configuration — mirrors rider's
-- `Domain.Types.RiderConfig.ExternalSOSConfig`. Rider's callbacks convert
-- between the two field-by-field; driver populates `Nothing`.
data ExternalSOSConfig = ExternalSOSConfig
  { flow :: ExternalSOSFlow,
    latLonRequired :: Bool,
    mediaRequired :: Bool,
    stateCode :: Maybe Text,
    tracePollingIntervalSeconds :: Maybe Seconds,
    trackingLinkRequired :: Bool,
    triggerSource :: ExternalSOSTriggerSource
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)

-- | Map a safety-domain ExternalSOSFlow to the Kernel SOS service enum.
-- Colocated with the ExternalSOSFlow type so they stay in sync.
flowToSOSService :: ExternalSOSFlow -> SOS.SOSService
flowToSOSService = \case
  ERSS -> SOS.ERSS
  GJ112 -> SOS.GJ112
  Trinity -> SOS.Trinity

$(mkBeamInstancesForEnumAndList ''RideShareOptions)

$(mkFromHttpInstanceForEnum ''RideShareOptions)
