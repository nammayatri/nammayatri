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
import Services.Backend as Remote
import Common.Types.App (LazyCheck(..))
import Services.API (UpdateProfileReq(..))

data FCMToken
  = FCMToken String

updateFirebaseToken :: Maybe String -> FlowBT String Unit
updateFirebaseToken deviceToken = do
  when (any (_ == deviceToken) [ Just "__f...led", Just "..." ]) $ do
    void $ liftFlowBT $ launchAff $ flowRunner defaultGlobalState
      $ do void $ runExceptT $ runBackT $ checkAndUpdateToken 5

checkAndUpdateToken :: Int -> FlowBT String Unit
checkAndUpdateToken retry =
  when (retry > 0) $ do
    (FCMToken newToken) <- lift $ lift $ doAff $ makeAff \cb -> setFCMTokenWithTimeOut 5000 (cb <<< Right) FCMToken $> nonCanceler
    if newToken == "NOT_FOUND" 
      then checkAndUpdateToken (retry - 1)
      else
        let (UpdateProfileReq initialData) = Remote.mkUpdateProfileRequest FunctionCall
            requiredData = initialData { deviceToken = Just newToken }
        in void $ lift $ lift $ Remote.updateProfile (UpdateProfileReq requiredData)
