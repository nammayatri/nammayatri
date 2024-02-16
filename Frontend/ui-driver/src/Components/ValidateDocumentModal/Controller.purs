
{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.ValidateDocumentModal.Controller where

import Common.Types.App (LazyCheck(..))
import Components.PrimaryButton.Controller as PrimaryButtonController
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Font.Size as FontSize
import Font.Style as FontStyle
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude (class Eq, class Show, (<>))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Padding(..), Visibility(..), height, width)
import Styles.Colors as Color
import Screens.Types as ST
import Helpers.Utils (fetchImage, FetchImageFrom(..))

data Action =  BackPressed 
              | AfterRender
              | PrimaryButtonActionController PrimaryButtonController.Action 

type ValidateDocumentModalState = {
  background :: String,
  imageBase64 :: String,
  headerImage :: String,
  profilePictureCapture :: Boolean,
  verificationStatus :: ST.ValidationStatus ,
  failureReason :: String,
  headerConfig :: HeadConfig,
  verificationType :: String
  }

type HeadConfig = {
  padding :: Padding,
  imageConfig :: TextConfig ,
  headTextConfig :: TextConfig
  }
type TextConfig = {
  text :: String , 
  fontSize :: Int , 
  focusIndex :: Int , 
  fontStyle :: String , 
  gravity :: Gravity , 
  visibility :: Visibility , 
  color :: String , 
  height :: Length , 
  width :: Length , 
  cornerRadius :: Number , 
  padding :: Padding , 
  margin :: Margin , 
  weight :: Number , 
  alpha :: Number
  }

config :: ValidateDocumentModalState
config = {
  background : Color.white900,
  profilePictureCapture : false,
  imageBase64 : fetchImage FF_COMMON_ASSET "ny_ic_profile_image",
  verificationStatus : ST.None, 
  verificationType : "",
  headerImage : fetchImage FF_ASSET "ny_ic_chevron_left_white", 
  failureReason : "",
  headerConfig : {
    padding : (Padding 5 16 5 16), 
    imageConfig : {
      text : "" ,
      fontSize : FontSize.a_14 ,
      focusIndex : 0 ,
      fontStyle : FontStyle.semiBold LanguageStyle ,
      gravity : CENTER ,
      visibility : VISIBLE ,
      color : Color.white900 ,
      height : V 30 ,
      width : V 30 ,
      cornerRadius : 0.0 ,
      padding : (Padding 2 2 2 2) ,
      margin : (MarginLeft 5) ,
      weight : 1.0 ,
      alpha : 0.0 
    } ,
    headTextConfig : {
      text : "" , 
      fontSize : FontSize.a_20 , 
      focusIndex : 0 , 
      fontStyle : FontStyle.semiBold LanguageStyle , 
      gravity : CENTER , 
      visibility : VISIBLE , 
      color : Color.white900 , 
      height : WRAP_CONTENT , 
      width : WRAP_CONTENT , 
      cornerRadius : 0.0 , 
      padding : (Padding 0 0 0 0) , 
      margin : (Margin 18 2 0 3) , 
      weight : 1.0 , 
      alpha : 0.0
    } 
  } 
}
