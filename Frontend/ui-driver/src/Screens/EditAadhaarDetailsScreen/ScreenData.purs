{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.EditAadhaarDetailsScreen.ScreenData where

import Screens.Types (EditAadhaarDetailsScreenState)
import Prelude (class Eq)
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)

initData :: EditAadhaarDetailsScreenState
initData =
  { data: {}
  , props:
      { isInEditAadharDetailsScreen: false
      }
  }

data ListOptions
  = AADHAAR_NUMBER
  | IMAGE_FRONT_SIDE
  | IMAGE_BACK_SIDE

derive instance genericListOptions :: Generic ListOptions _

instance eqListOptions :: Eq ListOptions where
  eq = genericEq

type Listtype
  = { value :: String
    , title :: ListOptions
    }

viewsItemList :: Array Listtype
viewsItemList =
  [ { title: AADHAAR_NUMBER, value: "" }
  , { title: IMAGE_FRONT_SIDE, value: "image.png" }
  , { title: IMAGE_BACK_SIDE, value: "image.png" }
  ]
