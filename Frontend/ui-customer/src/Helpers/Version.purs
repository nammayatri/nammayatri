{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Helpers.Version where

import Prelude
import Types.App (FlowBT, ScreenType(..), APP_UPDATE_POPUP(..))
import JBridge as JB
import MerchantConfig.Utils (Merchant(..), getMerchant)
import Screens.AppUpdatePopUp.Handler as UI
import Control.Monad.Except.Trans (lift)
import Engineering.Helpers.Commons (liftFlow, os, stringToVersion)
import Engineering.Helpers.LogEvent (logEvent)
import Foreign (unsafeToForeign)
import Common.Types.App (LazyCheck(..), Version(..))
import Data.Int as INT
import Data.Maybe (fromMaybe)
import Data.String (split, Pattern(..))
import Engineering.Helpers.BackTrack (liftFlowBT)
import ModifyScreenState
import Screens.Types (UpdatePopupType(..))
import Data.Array ((!!), all, any)
import Data.Maybe (isJust, Maybe(..), fromMaybe)
import Storage (getValueToLocalStore, KeyStore(..))
import Services.Backend as Remote
import Services.API (UpdateProfileReq(..))
import Data.Lens ((^.))
import Accessor (_minor, _major, _maintenance)
import Constants as Constants
import Domain.Cache (getAppConfigFromCache)

type IosVersion = {
  majorUpdateIndex :: Int,
  minorUpdateIndex :: Int,
  patchUpdateIndex :: Int,
  enableForceUpdateIOS :: Boolean
}

checkVersion :: FlowBT String Unit
checkVersion = do
  versionCodeAndroid <- liftFlowBT $ JB.getVersionCode
  versionName <- liftFlowBT $ JB.getVersionName

  let updatedIOSversion = getIosVersion (getMerchant FunctionCall)

  if androidUpdateRequired versionCodeAndroid 
    then do
      liftFlowBT $ JB.hideLoader
      config <- getAppConfigFromCache
      modifyScreenState $ AppUpdatePopUpScreenType (\appUpdatePopUpScreenState → appUpdatePopUpScreenState {updatePopup = AppVersion, config = config}) --TODO:: Make update popUp screen generic if it's used for other purposes also
      appUpdatedFlow <- UI.handleAppUpdatePopUp
      case appUpdatedFlow of
        Later -> pure unit
        _ -> checkVersion
    else if iosUpdateRequired updatedIOSversion.enableForceUpdateIOS versionName 
      then do
      let {majorUpdateIndex, minorUpdateIndex, patchUpdateIndex} = iosVersionSplit versionName
      when (
        all (_ /= -1) [majorUpdateIndex, minorUpdateIndex, patchUpdateIndex]
        && forceIOSupdate majorUpdateIndex minorUpdateIndex patchUpdateIndex updatedIOSversion
      ) $ do
          liftFlowBT $ JB.hideLoader
          void $ UI.handleAppUpdatePopUp
          checkVersion
      else pure unit

  where 
    iosVersionSplit :: String -> {majorUpdateIndex :: Int, minorUpdateIndex :: Int, patchUpdateIndex :: Int}
    iosVersionSplit versionName =
      let versionArray = (split (Pattern ".") versionName)
          majorUpdateIndex = versionIndex versionArray 0
          minorUpdateIndex = versionIndex versionArray 1
          patchUpdateIndex = versionIndex versionArray 2
      in {majorUpdateIndex, minorUpdateIndex, patchUpdateIndex}

    androidUpdateRequired :: Int -> Boolean
    androidUpdateRequired versionCodeAndroid = os /= "IOS" && versionCodeAndroid < (getLatestAndroidVersion (getMerchant FunctionCall))

    iosUpdateRequired :: Boolean  -> String -> Boolean
    iosUpdateRequired forceUpdate versionName = os == "IOS" && versionName /= "" && forceUpdate

    versionIndex :: Array String -> Int -> Int
    versionIndex versionArray index = fromMaybe (-1) $ INT.fromString =<< versionArray !! index

updateVersion :: Maybe Version -> Maybe Version -> FlowBT String Unit
updateVersion dbClientVersion dbBundleVersion = do
  if isJust dbClientVersion && isJust dbBundleVersion 
    then do
      let versionName = getValueToLocalStore VERSION_NAME
          bundle = getValueToLocalStore BUNDLE_VERSION
          clientVersion' = stringToVersion versionName
          bundleVersion' = stringToVersion bundle
          {clientVersion, bundleVersion} = getAppVersions dbClientVersion dbBundleVersion clientVersion' bundleVersion'
      when (
        versionValid clientVersion bundleVersion 
        && ( bundleVersion' /= bundleVersion || clientVersion' /= clientVersion )
      ) $ do
          let (UpdateProfileReq initialData) = Remote.mkUpdateProfileRequest FunctionCall
              requiredData = initialData{clientVersion = Just clientVersion, bundleVersion = Just bundleVersion}
          resp <- lift $ lift $ Remote.updateProfile (UpdateProfileReq requiredData)
          pure unit
  else pure unit
  where 
    getAppVersions :: Maybe Version -> Maybe Version -> Version -> Version -> {clientVersion :: Version, bundleVersion :: Version}
    getAppVersions dbClientVersion dbBundleVersion clientVersion' bundleVersion' =
      let clientVersion = fromMaybe clientVersion' dbClientVersion
          bundleVersion = fromMaybe bundleVersion' dbBundleVersion
      in {clientVersion, bundleVersion}
    
    versionValid :: Version -> Version -> Boolean
    versionValid clientVersion bundleVersion =
      let cvMinor = clientVersion ^. _minor
          cvMajor = clientVersion ^. _major
          cvMaintenance = clientVersion ^. _maintenance
          bvMinor = bundleVersion ^. _minor
          bvMajor = bundleVersion ^. _major
          bvMaintenance = bundleVersion ^. _maintenance 
      in all (_ /= -1) [cvMinor, cvMajor, cvMaintenance, bvMinor, bvMajor, bvMaintenance]

-- IOS latest version : 1.2.4
getIosVersion :: Merchant -> IosVersion
getIosVersion merchant =
  case merchant of
    NAMMAYATRI  -> mkIOSVersion 1 2 4 false
    YATRI       -> mkIOSVersion 1 1 0 true
    YATRISATHI  -> mkIOSVersion 0 1 0 false
    _           -> mkIOSVersion 0 1 0 false
  where 
    mkIOSVersion :: Int -> Int -> Int -> Boolean -> IosVersion
    mkIOSVersion majorUpdateIndex minorUpdateIndex patchUpdateIndex enableForceUpdateIOS =
      { majorUpdateIndex, minorUpdateIndex, patchUpdateIndex, enableForceUpdateIOS }

getLatestAndroidVersion :: Merchant -> Int
getLatestAndroidVersion merchant =
  case merchant of
    NAMMAYATRI -> 31
    YATRI -> 49
    YATRISATHI -> 105
    _ -> 1

forceIOSupdate :: Int -> Int -> Int -> IosVersion -> Boolean
forceIOSupdate c_maj c_min c_patch updatedIOSversion=
  c_maj < updatedIOSversion.majorUpdateIndex ||
  c_min < updatedIOSversion.minorUpdateIndex ||
  c_patch < updatedIOSversion.patchUpdateIndex