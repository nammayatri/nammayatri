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

genericHeaderConfig :: ST.InvoiceScreenState -> GenericHeader.Config
genericHeaderConfig state = let
    config = GenericHeader.config
    genericHeaderConfig' = config
       { height = WRAP_CONTENT
       , prefixImageConfig
       { height = V 25
       , width = V 25
       , imageUrl = "ny_ic_chevron_left,https://assets.juspay.in/nammayatri/images/common/ny_ic_chevron_left.png"
       , margin = (Margin 12 12 12 12)
       , visibility = VISIBLE
       }
    , textConfig
      { text = (getString INVOICE)
      , textSize = FontSize.a_18
      , color = Color.darkDescriptionText
      , fontStyle = FontStyle.bold LanguageStyle
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
     { textConfig
      { text = (getString DOWNLOAD_PDF)
      }
     }
   in primaryButtonConfig'
