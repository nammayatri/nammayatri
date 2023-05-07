module Screens.OnBoardingFlow.Proxy where

import Prelude
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Common.Types.App (GlobalPayload)
import Types.App (FlowBT)
import Presto.Core.Types.Language.Flow (doAff)
import Control.Monad.Except.Trans (lift)
import Types.App (HOME_SCREEN_OUTPUT)
-- import Screens.OnBoardingFlow.Flow (ScreenOutput(..))
-- import Screens.EnterMobileNumberScreen.Controller (ScreenOutput(..))

-- foreign import dynamicImport :: forall a. String -> EffectFnAff a
foreign import dynamicImport :: forall a. String -> EffectFnAff a

-- enterMobileNumberScreen :: FlowBT String ScreenOutput
-- enterMobileNumberScreen = do
--     func <- lift $ lift $ doAff $ fromEffectFnAff $ dynamicImport "enterMobileNumberScreen" -- (fnProxy HomeScreenHandler.homeScreen)
--     func

enterMobileNumberScreenFlow :: FlowBT String Unit
enterMobileNumberScreenFlow = do
    func <- lift $ lift $ doAff $ fromEffectFnAff $ dynamicImport "enterMobileNumberScreenFlow" -- (fnProxy HomeScreenHandler.homeScreen)
    func

chooseLanguageScreenFlow :: FlowBT String Unit
chooseLanguageScreenFlow = do
    func <- lift $ lift $ doAff $ fromEffectFnAff $ dynamicImport "chooseLanguageScreenFlow" -- (fnProxy HomeScreenHandler.homeScreen)
    func

accountSetUpScreenFlow :: FlowBT String Unit
accountSetUpScreenFlow = do
    func <- lift $ lift $ doAff $ fromEffectFnAff $ dynamicImport "accountSetUpScreenFlow" -- (fnProxy HomeScreenHandler.homeScreen)
    func

permissionScreenFlow :: String -> FlowBT String Unit
permissionScreenFlow a0 = do
    func <- lift $ lift $ doAff $ fromEffectFnAff $ dynamicImport "permissionScreenFlow" -- (fnProxy HomeScreenHandler.homeScreen)
    func a0