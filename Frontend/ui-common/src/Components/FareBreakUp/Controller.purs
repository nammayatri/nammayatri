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