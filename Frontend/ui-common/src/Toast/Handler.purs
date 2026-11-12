{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Toast.Handler where

import Data.Maybe (Maybe(..))
import Effect.Class (liftEffect)
import Prelude (Unit, bind, discard, pure, unit, ($), (>>=), void)
import Presto.Core.Flow (Flow)
import Presto.Core.Types.Language.Flow (doAff, getState)
import PrestoDOM.Core.Types.Language.Flow (initUIWithNameSpace, showScreenWithNameSpace, initUIWithScreen)
import Toast.View as ToastScreen
import Types.App (GlobalState(..))
import Engineering.Helpers.Commons (getGlobalPayload)
import Engineering.Helpers.Accessor
import Data.Lens ((^.))
import PrestoDOM.Core (terminateUI)

toastScreen :: Flow GlobalState Unit
toastScreen  = do
  (GlobalState state) <- getState
  let globalPayload = getGlobalPayload "__payload"
  case globalPayload of
    Nothing -> doAff $ liftEffect $ initUIWithNameSpace "Toast" Nothing
    Just payload -> doAff $ liftEffect $ initUIWithNameSpace "Toast" ((payload ^. _payload) ^. _fragmentViewGroups >>= (\a -> a ^. _main))
  _ <- showScreenWithNameSpace ( ToastScreen.screen state.toast)
  void $ doAff $ liftEffect $ terminateUI $ Just "Toast"
  pure unit