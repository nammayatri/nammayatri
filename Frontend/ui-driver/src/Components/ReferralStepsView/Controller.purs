{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.ReferralStepsView.Controller where

import Data.Tuple (Tuple(..))
import Prelude
import Styles.Colors as Color
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)

instance showAction :: Show Action where
  show (GoBack) = "GoBack"

data Action = GoBack

type Config = {
  referralSteps :: Array StepConfig,
  highlightTitle :: Boolean,
  heading :: String
}

type StepConfig = {
  icon :: String, 
  title :: String,
  background :: String,
  status :: Status
}

data Status = Pending | Done | InProgress

derive instance genericStatus :: Generic Status _
instance showStatus :: Show Status where show = genericShow
instance eqStatus :: Eq Status where eq = genericEq

config :: Config
config = 
  { referralSteps: [] 
  , highlightTitle: false
  , heading: ""
  }