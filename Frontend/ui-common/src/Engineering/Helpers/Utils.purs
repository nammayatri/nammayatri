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
import Debug (spy)


toggleLoader :: Boolean -> Flow GlobalState Unit
toggleLoader flag = do
  _ <- pure $ spy "toggleLoader" flag
  if flag then do
    state <- getState
    _ <- pure $ spy "inside toggleLoader" flag
    _ <- liftFlow $ launchAff $ flowRunner state UI.loaderScreen
    pure unit
    else do
      -- pure unit
      _ <- pure $ spy "inside toggleLoader" flag
      doAff $ liftEffect $ terminateUI $ Just "LoaderOverlay"

loaderText :: String -> String -> Flow GlobalState Unit
loaderText mainTxt subTxt = do 
  _ <- modifyState (\(GlobalState state) -> GlobalState state{loaderOverlay{data{title = mainTxt, subTitle = subTxt}}})
  pure unit