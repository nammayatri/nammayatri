{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Components.RideAllocationModal.Controller where

import Prelude (unit, (*), (/))
import PrestoDOM (Length(..), Margin(..))
import Font.Style as FontStyle
import Styles.Colors as Color
import Font.Size as FontSize
import Engineering.Helpers.Commons (screenWidth, screenHeight)
import Common.Types.App
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import Common.Types.App (LazyCheck(..))
import Prelude ((<>))

data Action
  = Decline String
  | Request String Number
  | CountDown Int String String String
  | NoAction
  | IncreasePrice String
  | DecreasePrice String

type Config
  = { id :: String
    , seconds :: Int
    , width :: Length
    , height :: Length
    , margin :: Margin
    , background :: String
    , countDown :: CountDownConfig
    , title :: TitleConfig
    , source :: SourceConfig
    , destination :: DestinationConfig
    , totalPrice :: Number
    , basePrice :: Number
    , reducePrice :: Number
    , increasePrice :: Number
    , decline :: DeclineConfig
    , request :: RequestConfig
    , journeyDistance :: Int
    , pickupDistance :: Int
    , destinationArea :: DestinationAreaConfig
    }

type CountDownConfig
  = { width :: Length
    , height :: Length
    , margin :: Margin
    , cornerRadius :: Number
    , background :: String
    , text :: String
    , textSize :: Int
    , textColor :: String
    }

type DeclineConfig
  = { height :: Length
    , width :: Length
    , cornerRadius :: Number
    , color :: String
    , background :: String
    }

type RequestConfig
  = { height :: Length
    , width :: Length
    , cornerRadius :: Number
    , color :: String
    , background :: String
    }

type DestinationConfig
  = { imageUrl :: String
    , imageWidth :: Length
    , imageHeight :: Length
    , text :: String
    , textSize :: Int
    , fontStyle :: String
    , textColor :: String
    }

type SourceConfig
  = { imageUrl :: String
    , imageWidth :: Length
    , imageHeight :: Length
    , text :: String
    , textSize :: Int
    , fontStyle :: String
    , textColor :: String
    }

type TitleConfig
  = { text :: String
    , color :: String
    , textSize :: Int
    , fontStyle :: String
    }

type DestinationAreaConfig
  = { text :: String
    , textSize :: Int
    , fontStyle :: String
    , textColor :: String
    }

config :: Config
config =
  { id: ""
  , seconds: 10
  , width: (V ((screenWidth unit) / 5 * 4))
  , height: WRAP_CONTENT
  , margin: (Margin ((screenWidth unit) / 20) ((screenHeight unit) / 5) ((screenWidth unit) / 20) 0)
  , background: Color.white900
  , countDown:
      { width: (V 40)
      , height: (V 40)
      , margin: (Margin ((screenWidth unit) / 20) ((screenHeight unit) / 5) ((screenWidth unit) / 20) 0)
      , cornerRadius: 20.0
      , background: Color.black
      , text: "1"
      , textSize: FontSize.a_20
      , textColor: Color.black
      }
  , title:
      { text: ""
      , color: Color.black
      , textSize: FontSize.a_19
      , fontStyle: FontStyle.medium LanguageStyle
      }
  , source:
      { imageUrl: fetchImage FF_COMMON_ASSET "ny_ic_source_dot"
      , imageWidth: (V 10)
      , imageHeight: (V 10)
      , text: ""
      , textSize: FontSize.a_14
      , fontStyle: FontStyle.regular LanguageStyle
      , textColor: Color.black800
      }
  , destination:
      { imageUrl: fetchImage FF_ASSET "ny_ic_destination"
      , imageWidth: (V 10)
      , imageHeight: (V 10)
      , text: ""
      , textSize: FontSize.a_14
      , fontStyle: FontStyle.regular LanguageStyle
      , textColor: Color.black800
      }
  , totalPrice: 0.0
  , basePrice: 0.0
  , reducePrice: 0.0
  , increasePrice: 0.0
  , decline:
      { width: (V ((screenWidth unit) / 9 * 3))
      , height: (V ((screenHeight unit) / 16))
      , cornerRadius: 5.0
      , color: Color.greyTextColor
      , background: Color.white900
      }
  , request:
      { width: (V ((screenWidth unit) / 9 * 3))
      , height: (V ((screenHeight unit) / 16))
      , cornerRadius: 5.0
      , color: Color.white900
      , background: Color.darkGreen
      }
  , journeyDistance: 0
  , pickupDistance: 0
  , destinationArea:
      { text: ""
      , textSize: FontSize.a_18
      , fontStyle: FontStyle.bold LanguageStyle
      , textColor: Color.greyTextColor
      }
  }
