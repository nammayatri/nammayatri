module PopUpOverlay.Types where

import Prelude
import Language.Strings (getString)
import Language.Types (STR(..))
import Foreign.Object (empty)
import ConfigProvider
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Foreign (Foreign)
import Foreign.Class (class Decode, class Encode)
import Foreign.Object (Object)
import MerchantConfig.Types

type PopUpOverlayState =
 { version :: Int ,
   logField :: Object Foreign,
   updatePopup :: PopupOverlayType,
   appUpdatedView :: AppUpdatedViewState,
   config :: AppConfig
 }

type AppUpdatedViewState = {
  primaryText :: String,
  secondaryText :: String,
  optionTwoText :: String,
  coverImageUrl :: String
}

data PopupOverlayType =  AppUpdate
                      | DateAndTime
                      | NoUpdate
                      | AppUpdated

derive instance genericPopupOverlayType :: Generic PopupOverlayType _
instance showPopupOverlayType :: Show PopupOverlayType where show = genericShow
instance eqPopupOverlayType :: Eq PopupOverlayType where eq = genericEq