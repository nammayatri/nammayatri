{-
  Copyright 2022-23, Juspay India Pvt Ltd
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.FavDriverGotIt.View where

import Components.FavDriverGotIt.Controller (Action(..), FavDriverConfig)
import Effect (Effect)
import Prelude (Unit)
import PrestoDOM (PrestoDOM, linearLayout)
import Effect.Class.Console (error)


view :: forall w. (Action -> Effect Unit) -> FavDriverConfig -> PrestoDOM ( Effect Unit ) w
view push state = linearLayout[] []