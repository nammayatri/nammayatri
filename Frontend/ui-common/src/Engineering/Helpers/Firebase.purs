{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Helpers.Firebase where

import Prelude
import Types.App (FlowBT, defaultGlobalState)
import Data.Maybe (Maybe(..))
import Data.Array (any)
import Control.Monad.Except.Trans (lift, runExceptT)
import Engineering.Helpers.BackTrack (liftFlowBT)
import Effect.Aff (launchAff, makeAff, nonCanceler)
import Engineering.Helpers.Commons (flowRunner)
import Presto.Core.Types.Language.Flow (doAff)
import Control.Transformers.Back.Trans (runBackT)
import JBridge (setFCMTokenWithTimeOut)
import Data.Either (Either(..))
import Effect.Uncurried (runEffectFn2)

updateFirebaseToken :: Maybe String -> (String -> FlowBT String Unit) -> FlowBT String Unit
updateFirebaseToken deviceToken updateToken = do
  when (any (_ == deviceToken) [ Just "__f...led", Just "..." ]) $ do
    void $ liftFlowBT $ launchAff $ flowRunner defaultGlobalState
      $ do void $ runExceptT $ runBackT $ checkAndUpdateToken 5 updateToken

checkAndUpdateToken :: Int -> (String -> FlowBT String Unit) -> FlowBT String Unit
checkAndUpdateToken retry updateToken =
  when (retry > 0) $ do
    newToken <- lift $ lift $ doAff $ makeAff \cb -> runEffectFn2 setFCMTokenWithTimeOut 5000 (cb <<< Right) $> nonCanceler
    if newToken == "NOT_FOUND" 
      then checkAndUpdateToken (retry - 1) updateToken
      else
        updateToken newToken
