module Screens.EarningsScreen.Common.Types where

import Prelude
import Common.Types.App
import Data.Maybe
import Effect
import Prelude
import PrestoDOM
import Engineering.Helpers.Commons as EHC
import Language.Strings (getString)
import Language.Types (STR(..))
import Styles.Colors as Color
import Prelude (class Eq)
import Data.Generic.Rep (class Generic)
import Data.Eq.Generic (genericEq)
import Services.API

type Layout e
  = PrestoDOM (Effect Unit) e

type TabList
  = { string :: STR
    , "type" :: EarningsTab
    }

data EarningsTab
  = TAB_DAILY
  | TAB_WEEKLY

derive instance genericEarningsTab :: Generic EarningsTab _

instance eqEarningsTab :: Eq EarningsTab where
  eq = genericEq

type RideComponent
  = { serviceTierType :: String
    , date :: String
    , time :: String
    , price :: String
    , tags :: Array RideTag
    , isCancelledRide :: Boolean
    }

type RideTag
  = { background :: String
    , color :: String
    , text :: String
    }


type RideData = {
  earnings :: Int,
  earningsWithCurrency :: Price,
  rideDistance :: Int,
  rideDate :: String,
  noOfRides :: Int,
  rideDistanceWithUnit :: Distance,
  list :: Array RidesInfo,
  noSummaryFound :: Boolean,
  noListFetched :: Boolean,
  noRidesTaken :: Boolean
}