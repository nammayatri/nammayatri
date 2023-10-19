{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module LoaderOverlay.Handler where

import Data.Maybe (Maybe(..))
import Effect.Class (liftEffect)
import Prelude (Unit, bind, discard, pure, unit, ($))
import Presto.Core.Flow (Flow)
import Presto.Core.Types.Language.Flow (doAff, getState)
import PrestoDOM.Core.Types.Language.Flow (initUIWithNameSpace, showScreenWithNameSpace, initUIWithScreen)
import LoaderOverlay.View as LoaderScreen
import Types.App (GlobalState(..))

loaderScreen :: Flow GlobalState Unit
loaderScreen  = do
  (GlobalState state) <- getState
  doAff $ liftEffect $ initUIWithNameSpace "LoaderOverlay" Nothing
  _ <- showScreenWithNameSpace ( LoaderScreen.screen state.loaderOverlay)
  pure unit