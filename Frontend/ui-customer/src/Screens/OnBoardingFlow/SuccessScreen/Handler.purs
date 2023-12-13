{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.SuccessScreen.Handler where

import Prelude (Unit, bind, pure, unit, ($))
import Control.Monad.Except.Trans (lift)
import PrestoDOM.Core.Types.Language.Flow (runScreenWithNameSpace, initUIWithNameSpace)
import Effect.Class (liftEffect)
import Presto.Core.Types.Language.Flow (doAff)
import Types.App (FlowBT)
import Screens.SuccessScreen.View as SuccessScreen
import Data.Maybe (Maybe(..))
import PrestoDOM.Core (terminateUI)

successScreen :: String -> String -> FlowBT String Unit
successScreen title subTitle = do
  _ <- lift $ lift $ doAff $ liftEffect $ initUIWithNameSpace "SuccessScreen" Nothing
  act <- lift $ lift $ runScreenWithNameSpace (SuccessScreen.screen { title: title, subTitle: subTitle })
  _ <- lift $ lift $ doAff $ liftEffect $ terminateUI $ Just "SuccessScreen"
  pure unit
