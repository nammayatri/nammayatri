{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.PublicTransportQuote where

import Domain.Types.PublicTransportQuote.RouteInfo (RouteInfo)
import Domain.Types.PublicTransportQuote.Stop (Stop)
import Kernel.Prelude
import Kernel.Types.Id

data PublicTransportQuote = PublicTransportQuote
  { id :: Id PublicTransportQuote,
    quoteId :: Text,
    start :: Stop,
    end :: Stop,
    routeInfo :: RouteInfo,
    totalEstimatedDistance :: Text,
    totalDuration :: Text,
    vehicleServiceType :: Text,
    stops :: [Stop],
    createdAt :: UTCTime
  }
  deriving (Generic, Show)

newtype PublicTransportQuoteAPIEntity = PublicTransportQuoteAPIEntity
  { quoteId :: Text
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)
