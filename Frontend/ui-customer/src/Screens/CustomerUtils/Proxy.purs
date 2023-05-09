module Screens.CustomerUtils.Proxy where

import Prelude
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Common.Types.App (GlobalPayload)
import Types.App (FlowBT)
import Presto.Core.Types.Language.Flow (doAff)
import Control.Monad.Except.Trans (lift)
import Debug (spy)

-- foreign import dynamicImport :: forall a. String -> EffectFnAff a
foreign import dynamicImport :: forall a. String -> EffectFnAff a

aboutUsScreenFlow :: FlowBT String Unit
aboutUsScreenFlow = do
    func <- lift $ lift $ doAff $ fromEffectFnAff $ dynamicImport "aboutUsScreenFlow" -- (fnProxy HomeScreenHandler.homeScreen)
    func

selectLanguageScreenFlow :: FlowBT String Unit
selectLanguageScreenFlow = do
    func <- lift $ lift $ doAff $ fromEffectFnAff $ dynamicImport "selectLanguageScreenFlow" -- (fnProxy HomeScreenHandler.homeScreen)
    func

helpAndSupportScreenFlow :: FlowBT String Unit
helpAndSupportScreenFlow = do
    func <- lift $ lift $ doAff $ fromEffectFnAff $ dynamicImport "helpAndSupportScreenFlow" -- (fnProxy HomeScreenHandler.homeScreen)
    func

invoiceScreenFlow :: FlowBT String Unit
invoiceScreenFlow = do
    func <- lift $ lift $ doAff $ fromEffectFnAff $ dynamicImport "invoiceScreenFlow" -- (fnProxy HomeScreenHandler.homeScreen)
    func

myProfileScreenFlow :: FlowBT String Unit
myProfileScreenFlow = do
    func <- lift $ lift $ doAff $ fromEffectFnAff $ dynamicImport "myProfileScreenFlow" -- (fnProxy HomeScreenHandler.homeScreen)
    func

emergencyScreenFlow :: FlowBT String Unit
emergencyScreenFlow = do
    func <- lift $ lift $ doAff $ fromEffectFnAff $ dynamicImport "emergencyScreenFlow" -- (fnProxy HomeScreenHandler.homeScreen)
    func

addNewAddressScreenFlow :: String -> FlowBT String Unit
addNewAddressScreenFlow a0 = do
    func <- lift $ lift $ doAff $ fromEffectFnAff $ dynamicImport "addNewAddressScreenFlow" -- (fnProxy HomeScreenHandler.homeScreen)
    func a0

tripDetailsScreenFlow :: Boolean -> FlowBT String Unit
tripDetailsScreenFlow a0 = do
    func <- lift $ lift $ doAff $ fromEffectFnAff $ dynamicImport "tripDetailsScreenFlow" -- (fnProxy HomeScreenHandler.homeScreen)
    func a0

myRidesScreenFlow :: Boolean -> FlowBT String Unit
myRidesScreenFlow a0 = do
    _ <- pure $ spy "bundle_split myRidesScreenFlow" "."
    func <- lift $ lift $ doAff $ fromEffectFnAff $ dynamicImport "myRidesScreenFlow" -- (fnProxy HomeScreenHandler.homeScreen)
    func a0