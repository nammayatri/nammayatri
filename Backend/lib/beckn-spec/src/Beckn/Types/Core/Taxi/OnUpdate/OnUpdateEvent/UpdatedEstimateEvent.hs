{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wwarn=incomplete-uni-patterns #-}

module Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.UpdatedEstimateEvent where

import Beckn.Types.Core.Taxi.Common.FulfillmentInfo
import qualified Beckn.Types.Core.Taxi.OnUpdate.Descriptor as Descriptor
import qualified Beckn.Types.Core.Taxi.OnUpdate.Provider as Provider
import Kernel.Prelude

data UpdatedEstimateEvent = UpdatedEstimateEvent
  { id :: Text, -- bppBookingId
    fulfillment :: FulfillmentInfo, -- to see
    bpp_providers :: NonEmpty Provider.Provider,
    bpp_descriptor :: Descriptor.Descriptor
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)
