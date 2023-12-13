{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.EditBankDetailsScreen.ScreenData where

import Screens.Types (EditBankDetailsScreenState)
import Prelude (class Eq)
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)

initData :: EditBankDetailsScreenState
initData =
  { data:
      {}
  , props:
      { isInEditBankDetailsScreen: false
      }
  }

data ListOptions
  = DRIVER_BANK_NAME
  | ACCOUNT_NO
  | IFSC

derive instance genericListOptions :: Generic ListOptions _

instance eqListOptions :: Eq ListOptions where
  eq = genericEq

type Listtype
  = { value :: String
    , title :: ListOptions
    }

viewsItemList :: Array Listtype
viewsItemList =
  [ { title: DRIVER_BANK_NAME, value: "" }
  , { title: ACCOUNT_NO, value: "" }
  , { title: IFSC, value: "" }
  ]
