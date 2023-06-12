module Engineering.Helpers.Utils where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff (launchAff)
import Effect.Class (liftEffect)
import Engineering.Helpers.Commons (flowRunner, liftFlow)
import LoaderOverlay.Handler as UI
import Presto.Core.Types.Language.Flow (Flow, doAff, getState, modifyState)
import PrestoDOM.Core (terminateUI)
import Types.App (GlobalState(..))


toggleLoader :: Boolean -> Flow GlobalState Unit
toggleLoader flag = if flag then do
  state <- getState
  _ <- liftFlow $ launchAff $ flowRunner state UI.loaderScreen
  pure unit
  else
    -- pure unit
    doAff $ liftEffect $ terminateUI $ Just "LoaderOverlay"

loaderText :: String -> String -> Flow GlobalState Unit
loaderText mainTxt subTxt = do 
  _ <- modifyState (\(GlobalState state) -> GlobalState state{loaderOverlay{data{title = mainTxt, subTitle = subTxt}}})
  pure unit