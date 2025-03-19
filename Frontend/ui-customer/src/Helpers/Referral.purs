module Helpers.Referral where

import Prelude
import Storage (KeyStore(..), getValueToLocalStore, setValueToLocalStore)
import Types.App (FlowBT)
import Screens.Types (ReferralStatus(..))
import Services.Backend as Remote
import Data.Either (Either(..))
import Common.Types.App (LazyCheck(..))
import Services.API (UpdateProfileReq(..))
import Data.Maybe (Maybe(..))
import Control.Monad.Except.Trans (lift)
import JBridge as JB
import Helpers.Utils (decodeError)
import ConfigProvider
import Helpers.Utils (getCityConfig)
import Mobility.Prelude (startsWith)

applyReferralCode :: String -> FlowBT String ReferralStatus
applyReferralCode referralCode  = do
  if getValueToLocalStore REFERRAL_STATUS == "NOT_REFERRED_NOT_TAKEN_RIDE" then do
    let (UpdateProfileReq initialData) = Remote.mkUpdateProfileRequest FunctionCall
        deviceId = JB.getDeviceID unit
        androidId = JB.getAndroidId unit
        mbDeviceId = if deviceId /= "NO_DEVICE_ID" then Just deviceId else Nothing
        mbAndroidId = if androidId /= "NO_ANDROID_ID" then Just androidId else Nothing
        requiredData = initialData{referralCode = (Just referralCode), deviceId = mbDeviceId, androidId = mbAndroidId}
    res <- lift $ lift $ Remote.updateProfile (UpdateProfileReq requiredData)
    case res of
      Right _ -> do
        setValueToLocalStore REFERRAL_STATUS "REFERRED_NOT_TAKEN_RIDE"  
        setValueToLocalStore REFERRER_URL ""
        void $ pure $ JB.cleverTapCustomEvent "ny_user_referral_code_applied"
        pure REFERRAL_APPLIED
      Left err -> do
        if ((err.code == 500 && (decodeError err.response.errorMessage "errorCode") == "BPP_INTERNAL_API_ERROR")
            || startsWith "Referral Code must" (decodeError err.response.errorMessage "errorMessage") 
            || decodeError err.response.errorMessage "errorMessage" == "Cannot refer yourself"
            || decodeError err.response.errorMessage "errorMessage" == "Invalid ReferralCode") then do
            setValueToLocalStore REFERRER_URL ""
            pure REFERRAL_INVALID
        else 
            pure NO_REFERRAL
  else
      pure NO_REFERRAL

generateReferralLink :: String -> String -> String -> String -> String -> String
generateReferralLink source medium term content campaign =
  let config = getAppConfig appConfig 
      cityConfig = getCityConfig config.cityConfig source
      path = "/refer"
      packageId = cityConfig.referral.customerAppId
      domain = cityConfig.referral.domain
  in domain <> path <> "?referrer=" 
      <> "utm_source%3D" <> source 
      <> "%26utm_medium%3D" <> medium 
      <> "%26utm_term%3D" <> term 
      <> "%26utm_content%3D" <> content 
      <> "%26utm_campaign%3D" <> campaign 
      <> "%26anid%3Dadmob&id=" <> packageId