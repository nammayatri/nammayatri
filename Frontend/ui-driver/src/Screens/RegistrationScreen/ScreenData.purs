module Screens.RegistrationScreen.ScreenData where

import Screens.Types (RegistrationScreenState)
import Prelude (class Eq)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep (class Generic)

data ListOptions = DRIVING_LICENSE_OPTION | VEHICLE_DETAILS_OPTION
derive instance genericListOptions :: Generic ListOptions _
instance eqListOptions :: Eq ListOptions where eq = genericEq

initData :: RegistrationScreenState
initData = {
      data: {},
      props: {}
    }

optionList :: Array ListOptions
optionList = [ DRIVING_LICENSE_OPTION, VEHICLE_DETAILS_OPTION ]