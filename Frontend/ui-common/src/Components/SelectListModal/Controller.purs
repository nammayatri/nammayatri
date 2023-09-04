{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.SelectListModal.Controller where

import Components.PrimaryButton.Controller as PrimaryButtonController
import Data.Maybe
import Common.Types.App (OptionButtonList)
import PrestoDOM ( Length(..) , Gravity(..), Margin(..), Padding(..))
import Font.Size as FontSize
import Styles.Colors as Color
import MerchantConfig.DefaultConfig as DC
import MerchantConfig.Types (AppConfig)


data Action = Button1 PrimaryButtonController.Action
            | Button2 PrimaryButtonController.Action
            | UpdateIndex Int
            | NoAction
            | OnGoBack
            | TextChanged String String
            | ClearOptions
            
type Config = {
    selectionOptions :: Array OptionButtonList
    , primaryButtonTextConfig :: ButtonTextConfig
    , headingTextConfig :: HeadingTextConfig
    , subHeadingTextConfig :: HeadingTextConfig
    , primaryButtonVisibility :: Boolean
    , secondaryButtonVisibility :: Boolean
    , activeIndex :: Maybe Int
    , cornerRadius :: Number
    , activeReasonCode :: Maybe String
    , isSelectButtonActive :: Boolean
    , isMandatoryTextHidden :: Boolean
    , isLimitExceeded :: Boolean
    , showAllOptionsText :: String
    , hint :: String
    , strings :: StringConfig
    , topLeftIcon :: Boolean
    , config :: AppConfig
    , hideOthers :: Boolean
    , showBgColor :: Boolean
    , editTextBgColor :: String
    , defaultText :: String
}

type StringConfig = {
      mandatory :: String 
    , limitReached :: String 
}

type ButtonTextConfig = 
  {
      firstText :: String
    , secondText :: String
    , width :: Length
  }

type HeadingTextConfig = 
  {
       text :: String
      , size :: Int
      , color :: String
      , visibility :: Boolean
      , gravity :: Gravity
      , margin :: Margin
      , padding :: Padding
  }


config :: Config
config = 
  {
    selectionOptions : []
    , showAllOptionsText : "Show All Options"
    , primaryButtonTextConfig :
        {
            firstText : "Go Back"
          , secondText : "Cancel Ride"
          , width : MATCH_PARENT
        }
    , activeIndex : Nothing  
    , activeReasonCode : Nothing
    , cornerRadius : 8.0
    , isSelectButtonActive : false
    , isMandatoryTextHidden : true
    , isLimitExceeded : false
    , headingTextConfig : 
      {
          text : ""
          , size : FontSize.a_22
          , color : Color.black800
          , visibility : true
          , gravity : CENTER 
          , margin : Margin 0 0 0 0
          , padding : Padding 0 0 0 0
      }
    , subHeadingTextConfig :
      {
          text : ""
          , size : FontSize.a_14
          , color : Color.black700
          , visibility : true
          , gravity : CENTER 
          , margin : Margin 0 0 0 0
          , padding : PaddingLeft 20
      }
    , hint : ""
    , strings: {
        mandatory : "Mandatory"
      , limitReached : "Max character limit reached 100 of 100"
    }
    , primaryButtonVisibility : true
    , secondaryButtonVisibility : true
    , topLeftIcon : false
    , config : DC.config
    , hideOthers : true
    , showBgColor : false
    , editTextBgColor : Color.grey800
    , defaultText : ""
}