{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module PopUpOverlay.Handler where

import Prelude (Unit, bind, discard, pure, unit, ($), (<$>))
import Presto.Core.Types.Language.Flow (doAff)
import PopUpOverlay.Controller as CD
import PopUpOverlay.View as PopUpOverlay
import PopUpOverlay.ScreenData as PopUpOverlayScreenData
import PrestoDOM.Core.Types.Language.Flow(runScreenWithNameSpace, initUIWithNameSpace)
import Types.App (FlowBT, GlobalState(..), APP_UPDATE_POPUP(..))
import Control.Monad.Except.Trans (lift)
import Effect.Class (liftEffect)
import Control.Transformers.Back.Trans (BackT(..), FailBack(..)) as App
import Engineering.Helpers.BackTrack (getState)
import Data.Maybe (Maybe(..))
import PrestoDOM.Core (terminateUI)
import Presto.Core.Types.Language.Flow (getLogFields)

showPopUpOverlay :: (FlowBT String Unit) -> FlowBT String Unit
showPopUpOverlay acceptFlow = do
  let state = PopUpOverlayScreenData.initData
  logField_ <-lift $ lift $ getLogFields
  _ <- lift $ lift $ doAff $ liftEffect $ initUIWithNameSpace "PopUpOverlay" Nothing 
  act <- lift $ lift $ runScreenWithNameSpace ( PopUpOverlay.screen state{logField = logField_})
  _ <- lift $ lift $ doAff $ liftEffect $ terminateUI $ Just "PopUpOverlay"
  case act of
    CD.Accept -> do 
      acceptFlow
      showPopUpOverlay acceptFlow
    CD.Decline -> pure unit
