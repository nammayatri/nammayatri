module RideBookingFlow.Dynamic where

import Prelude
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Common.Types.App (GlobalPayload)
import Types.App (FlowBT, HOME_SCREEN_OUTPUT(..))
import Presto.Core.Types.Language.Flow (doAff)
import Control.Monad.Except.Trans (lift)
-- import Screens.HomeScreen.Handler as HomeScreenHandler
-- import Screens.HomeScreen.View as HomeScreenView
-- import Screens.RideBookingFlow.Flow (HomeScreenView, HomeScreenHandl)
import Type.Proxy (Proxy(..))
import Screens.Types (HomeScreenState)

-- foreign import dView :: forall a.  -> EffectFnAff a
-- foreign import dController :: forall a. Proxy a -> EffectFnAff a
foreign import dHandler :: forall a. String -> EffectFnAff a
-- foreign import dTransformer :: forall a. Proxy a -> EffectFnAff a
-- foreign import dConfig :: forall a. Proxy a -> EffectFnAff a

fnProxy :: forall a. a ->  Proxy a
fnProxy _ = Proxy

homeScreen :: FlowBT String HOME_SCREEN_OUTPUT
homeScreen = do
    func <- lift $ lift $ doAff $ fromEffectFnAff $ dHandler "homeScreen" -- (fnProxy HomeScreenHandler.homeScreen)
    func

    -- screen :: HomeScreenState ->
    -- screen = do
    --     func <- lift $ lift $ doAff $ fromEffectFnAff $ dView "screen"--(fnProxy HomeScreenView.screen)
    --     func