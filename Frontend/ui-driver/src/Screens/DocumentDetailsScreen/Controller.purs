module Screens.DocumentDetailsScreen.Controller
  ( Action(..)
  , ScreenOutput(..)
  , eval
  )
  where

import Components.ChooseVehicle as ChooseVehicle
import Components.PrimaryButton as PrimaryButton
import Data.Array (filter, length, (!!))
import Data.Maybe (Maybe(..), fromMaybe)
import Log (trackAppScreenRender)
import Prelude (class Show, map, pure, show, unit, (<>), (==), not, ($), (>))
import PrestoDOM (Eval, update, continue, exit, continueWithCmd)
import PrestoDOM.Types.Core (class Loggable)
import Screens.Types (DocumentDetailsScreenState, VehicleP)
import Common.Types.App (LazyCheck(..))
import MerchantConfig.Utils (Merchant(..), getMerchant)
import Helpers.Utils (getVehicleVariantImage)
import Language.Strings (getString)
import Language.Types (STR(..))
import Components.AppOnboardingNavBar as AppOnboardingNavBar

instance showAction :: Show Action where
  show BackPressed = "BackPressed"
  show (AppOnboardingNavBarAC var1) = "AppOnboardingNavBarAC_" <> show var1

  
instance loggableAction :: Loggable Action where
  performLog action appId = case action of
    BackPressed -> pure unit
    _ -> pure unit

data Action = BackPressed
            | AppOnboardingNavBarAC AppOnboardingNavBar.Action

data ScreenOutput = GoBack | SelectCab DocumentDetailsScreenState Boolean

eval :: Action -> DocumentDetailsScreenState -> Eval Action ScreenOutput DocumentDetailsScreenState
eval BackPressed state = exit GoBack

eval (AppOnboardingNavBarAC (AppOnboardingNavBar.Logout)) state = continue state {props{menuOptions = not state.props.menuOptions}}

eval (AppOnboardingNavBarAC (AppOnboardingNavBar.PrefixImgOnClick) ) state = continueWithCmd state [ pure BackPressed]

eval _ state = update state
