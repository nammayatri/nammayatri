{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.CustomerUtils.InvoiceScreen.ComponentConfig where

import PrestoDOM (Length(..), Margin(..), Padding(..), Visibility(..))
import Screens.Types as ST
import Font.Size as FontSize
import Font.Style as FontStyle
import Language.Strings (getString)
import Language.Types (STR(..))
import Components.PrimaryButton as PrimaryButton
import Components.GenericHeader as GenericHeader
import Styles.Colors as Color
import Common.Types.App
import Prelude ((<>))
import Helpers.Utils (getAssetStoreLink, getCommonAssetStoreLink)
import Common.Types.App (LazyCheck(..))

genericHeaderConfig :: ST.InvoiceScreenState -> GenericHeader.Config
genericHeaderConfig state = let
    config = GenericHeader.config
    genericHeaderConfig' = config
       { height = WRAP_CONTENT
       , prefixImageConfig
       { height = V 25
       , width = V 25
       , imageUrl = "ny_ic_chevron_left," <> (getCommonAssetStoreLink FunctionCall) <> "ny_ic_chevron_left.png"
       , margin = (Margin 12 12 12 12)
       , visibility = VISIBLE
       }
    , textConfig
      { text = (getString INVOICE)
      , color = Color.darkDescriptionText
      }
    , suffixImageConfig
      { visibility = GONE
      }
    , padding = (Padding 0 5 0 5)
      }
    in genericHeaderConfig'

primaryButtonConfig :: ST.InvoiceScreenState -> PrimaryButton.Config
primaryButtonConfig state = let
   config = PrimaryButton.config
   primaryButtonConfig' = config
     { textConfig{ text = (getString DOWNLOAD_PDF)
      , accessibilityHint = ((getString DOWNLOAD_PDF) <> " : Button" )
      , color = state.data.config.primaryTextColor }
      , background = state.data.config.primaryBackground
     }
   in primaryButtonConfig'
