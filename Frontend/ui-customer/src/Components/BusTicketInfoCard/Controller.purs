{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.BusTicketInfoCard.Controller where

import Components.PrimaryButton as PrimaryButtonController
import Components.SourceToDestination as SourceToDestinationController
import Screens.Types(Stage, ZoneType(..), SearchResultType)
import Data.Maybe(Maybe)
import Components.ChatView as ChatView
import MerchantConfig.Types

data Action = NoAction
            | ShowTicketQR
            | ShowRouteInfo
            
type BusTicketInfoCardState =
  { props :: BusTicketInfoCardProps
  , data :: BusTicketInfoCardData
  }

type BusTicketInfoCardProps =
  { 
  }

type BusTicketInfoCardData =
  { busId :: String
  , quantity :: Int
  , registrationNumber :: String
  , routeNo :: String
  , busModel :: String
  }
