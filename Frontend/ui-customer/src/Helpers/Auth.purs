{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Helpers.Auth where

import Prelude
import Services.API (TriggerSignatureOTPResp(..), User(..))
import Data.Either (Either(..))
import Presto.Core.Types.API (ErrorResponse)
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import Engineering.Helpers.BackTrack (liftFlowBT)
import Storage (setValueToLocalStore, KeyStore(..), getValueToLocalStore)
import Effect.Uncurried (runEffectFn2)
import Control.Monad.Except.Trans (lift)
import Presto.Core.Types.Language.Flow (setLogField)
import Foreign.Class (encode)
import Helpers.Utils (getMobileNumber)
import Common.Types.App (SignatureAuthData(..), EventPayload(..))
import Types.App (FlowBT)
import Data.Function.Uncurried (runFn3)
import JBridge (emitJOSEvent)
import Accessor (_authData, _maskedMobileNumber, _id)
import Data.Lens ((^.))
import Engineering.Helpers.MobilityPrelude

validateSignaturePayload :: SignatureAuthData -> (Either ErrorResponse TriggerSignatureOTPResp) -> FlowBT String Boolean
validateSignaturePayload signatureAuth resp = 
  case resp of
    Right (TriggerSignatureOTPResp triggerSignatureOtpResp) -> do
      case triggerSignatureOtpResp.person of
        Just person -> do
          mobileNumber <- liftFlowBT $ runEffectFn2 getMobileNumber (signatureAuth ^._authData) $ fromMaybeString (person ^._maskedMobileNumber)
          updateCustomerDetails person mobileNumber
        _ -> pure unit
      case triggerSignatureOtpResp.token of
        Just token -> setValueToLocalStore REGISTERATION_TOKEN token
        Nothing -> pure unit
      pure true
    Left err -> do
      void $ liftFlowBT $ pure $ runFn3 emitJOSEvent "java" "onEvent" $ encode $ EventPayload { event: "signature_auth_failed", payload: Nothing }
      pure false
  where
    updateCustomerDetails :: User -> String -> FlowBT String Unit
    updateCustomerDetails person mobileNumber = do
      lift $ lift $ setLogField "customer_id" $ encode $ person^._id
      setValueToLocalStore CUSTOMER_ID $ person^._id
      setValueToLocalStore MOBILE_NUMBER mobileNumber



validateToken :: Maybe SignatureAuthData -> FlowBT String Boolean
validateToken signatureAuth =
  let
    registrationToken = getValueToLocalStore REGISTERATION_TOKEN
  in
    pure $ registrationToken /= "__failed" && registrationToken /= "(null)" && (isNothing signatureAuth)
