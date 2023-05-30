{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.LoaderScreen.Handler where

import Prelude

import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans as App
import Prelude (Unit, bind, ($), (<$>), pure)
import Presto.Core.Flow (Flow)
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Screens.LoaderScreen.Controller (ScreenOutput(..))
import Screens.LoaderScreen.View as LoaderScreen
import Types.App (FlowBT, GlobalState(..))


import Prelude (bind, pure, ($), (<$>))
import Presto.Core.Types.Language.Flow (doAff)
import PrestoDOM.Core.Types.Language.Flow(runScreenWithNameSpace, initUIWithNameSpace)
import Types.App (FlowBT, GlobalState(..))
import Control.Monad.Except.Trans (lift)
import Effect.Class (liftEffect)
import Control.Transformers.Back.Trans (BackT(..), FailBack(..)) as App
import Engineering.Helpers.BackTrack (getState)
import Data.Maybe (Maybe(..))
import PrestoDOM.Core (terminateUI)

loaderScreen :: forall a. Flow a Unit
loaderScreen  = do
  _ <- doAff $ liftEffect $ initUIWithNameSpace "LoaderOverlay" Nothing
  _ <- runScreenWithNameSpace ( LoaderScreen.screen {})
  pure unit