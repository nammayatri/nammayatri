module Screens.SuccessScreen.Handler where

import Prelude (Unit, bind, pure, unit, ($))
import Control.Monad.Except.Trans (lift)
import PrestoDOM.Core.Types.Language.Flow (runScreenWithNameSpace, initUIWithNameSpace)
import Effect.Class (liftEffect)
import Presto.Core.Types.Language.Flow (doAff)
import Types.App (FlowBT)
import Screens.SuccessScreen.View as SuccessScreen
import Data.Maybe (Maybe(..))
import PrestoDOM.Core2 (terminateUI)

successScreen :: String -> String -> FlowBT String Unit
successScreen title subTitle = do
  _ <- lift $ lift $ doAff $ liftEffect $ initUIWithNameSpace "SuccessScreen" Nothing
  act <- lift $ lift $ runScreenWithNameSpace (SuccessScreen.screen { title: title, subTitle: subTitle })
  _ <- lift $ lift $ doAff $ liftEffect $ terminateUI $ Just "SuccessScreen"
  pure unit
