{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.FareBreakUp.Controller where

import Font.Style as FontStyle
import Components.SourceToDestination.Controller as SourceToDestinationController
import Font.Size as FontSize
import Styles.Colors as Color
import PrestoDOM (Margin(..), Visibility(..))
import Common.Types.App
import Data.Maybe

data Action = NoAction 
            | SourceToDestinationActionController SourceToDestinationController.Action 
            | ShowInvoice 

type Config = 
  { fareDetails :: Array FareDetails
  , totalAmount :: FareDetails 
  , rideDetails :: RideDetails
  , headingText :: String 
  }
type FareDetails = 
  { text :: String 
  , textSize :: Int 
  , fontStyle :: String
  , color :: String 
  , margin :: Margin
  , priceDetails :: PriceDetails 
  , visibility :: Visibility
  }

type PriceDetails = 
  { text :: Int 
  , textSize :: Int 
  , fontStyle :: String 
  , offeredFare :: Int
  , distanceDifference :: Int
  }

type RideDetails = 
  { sourceTitle :: String 
  , source :: String 
  , destinationTitle :: String 
  , destination :: String 
  , rideStartTime :: String 
  , rideStartDate :: String 
  , estimatedDistance :: Maybe Int
  }

config :: Config 
config = 
  { fareDetails : []
  , headingText : "View Breakdown"
  , totalAmount : 
    { text : "Total Amount"
    , textSize : FontSize.a_16
    , fontStyle : FontStyle.medium LanguageStyle
    , color : Color.black700
    , visibility : VISIBLE
    , margin : (Margin 0 0 0 0)
    , priceDetails : {
        text : 0
      , textSize : FontSize.a_16 
      , fontStyle : FontStyle.medium LanguageStyle
      , offeredFare : 0
      , distanceDifference : 0
      }
    }
  , rideDetails : {
      sourceTitle : ""
    , source : ""
    , destination : ""
    , destinationTitle : ""
    , rideStartTime : ""
    , rideStartDate : ""
    , estimatedDistance : Nothing
  }
  }  