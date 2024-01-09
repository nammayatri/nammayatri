{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Engineering.Helpers.Version where

import Prelude
import Types.App (FlowBT, ScreenType(..), APP_UPDATE_POPUP(..))
import JBridge as JB
import MerchantConfig.Utils (Merchant(..), getMerchant)
import PopUpOverlay.Handler as UI
import Control.Monad.Except.Trans (lift)
import Engineering.Helpers.Commons (liftFlow, os, stringToVersion)
import Engineering.Helpers.LogEvent (logEvent)
import Engineering.Helpers.Utils (showSplash, reboot)
import Foreign (unsafeToForeign)
import Common.Types.App (LazyCheck(..), Version(..), FCMBundleUpdate(..))
import Data.Int as INT
import Data.Maybe (fromMaybe)
import Data.String (split, Pattern(..))
import Engineering.Helpers.BackTrack (getState, liftFlowBT)
import PopUpOverlay.Types (PopupOverlayType(..))
import Data.Array ((!!), all, any)
import Data.Maybe
import Storage (getValueToLocalStore, KeyStore(..))
import Services.Backend as Remote
import Services.API (UpdateProfileReq(..))
import Data.Lens ((^.))
import Accessor (_minor, _major, _maintenance)
import ConfigProvider
import Common.Types.Config
import Data.Int (fromString)

type IosVersion = {
  majorUpdateIndex :: Int,
  minorUpdateIndex :: Int,
  patchUpdateIndex :: Int,
  enableForceUpdateIOS :: Boolean
}

verifyAppUpdate :: FlowBT String Unit
verifyAppUpdate = do
  config <- getAppConfigFlowBT appConfig  
  maybe'
    (\_ -> pure unit) 
    (\appUpdate -> do
        versionCodeAndroid <- liftFlowBT $ JB.getVersionCode
        versionName <- liftFlowBT $ JB.getVersionName
        let versions = if os == "IOS" then appUpdate.ios else appUpdate.android
            versionCode = if os == "IOS" then Nothing else Just versionCodeAndroid
        if validateMinVersion versions versionCode versionName 
          then UI.showPopUpOverlay (pure unit)
          else 
            if validateForceUpdate versions versionCode versionName
            then UI.showPopUpOverlay (pure unit)
            else pure unit
          ) config.appUpdate
  



validateMinVersion :: Versions -> Maybe Int -> String -> Boolean
validateMinVersion version versionCode currentVersion = do
  case versionCode of
    Just code -> code >= (fromMaybe 0 $ fromString version.min)
    Nothing -> iosVersionSplit currentVersion >= iosVersionSplit version.min

validateForceUpdate :: Versions -> Maybe Int -> String -> Boolean
validateForceUpdate version versionCode currentVersion =
  let isLatestVerison = case versionCode of
        Just code -> code >= (fromMaybe 0 $ fromString version.latest)
        Nothing -> iosVersionSplit currentVersion >= iosVersionSplit version.latest
  in version.forceUpdate && (not isLatestVerison)


checkForUpdatesInBackground :: FlowBT String Unit
checkForUpdatesInBackground = do
  state <- getState
  void $ liftFlowBT $ launchAff $ flowRunner state $ do pure unit
      -- void $ lift $ lift $ delay $ Milliseconds 2000.0
      -- liftFlowBT $ terminateUI $ Just "SplashScreen"
-- do
--   versionCodeAndroid <- liftFlowBT $ JB.getVersionCode
--   versionName <- liftFlowBT $ JB.getVersionName
--   config <- getAppConfigFlowBT appConfig
--   let updatedIOSversion = getIosVersion config.latestVersion.ios
--   if androidUpdateRequired versionCodeAndroid config.latestVersion.android
--     then do
--       liftFlowBT $ JB.hideLoader
--       -- modifyScreenState $ AppUpdatePopUpScreenType (\appUpdatePopUpScreenState → appUpdatePopUpScreenState {updatePopup = AppUpdate}) --TODO:: Make update popUp screen generic if it's used for other purposes also
--       appUpdatedFlow <- UI.showPopUpOverlay
--       case appUpdatedFlow of
--         Later -> pure unit
--         _ -> checkVersion
--     else if iosUpdateRequired updatedIOSversion.enableForceUpdateIOS versionName 
--       then do
--       let {majorUpdateIndex, minorUpdateIndex, patchUpdateIndex} = iosVersionSplit versionName
--       when (
--         all (_ /= -1) [majorUpdateIndex, minorUpdateIndex, patchUpdateIndex]
--         && forceIOSupdate majorUpdateIndex minorUpdateIndex patchUpdateIndex updatedIOSversion
--       ) $ do
--           liftFlowBT $ JB.hideLoader
--           void $ UI.showPopUpOverlay
--           checkVersion
--       else pure unit

--   where 
--     androidUpdateRequired :: Int -> String Boolean
--     androidUpdateRequired versionCodeAndroid = os /= "IOS" && versionCodeAndroid < (getLatestAndroidVersion (getMerchant FunctionCall))

--     iosUpdateRequired :: Boolean  -> String -> Boolean
--     iosUpdateRequired forceUpdate versionName = os == "IOS" && versionName /= "" && forceUpdate

--     versionIndex :: Array String -> Int -> Int
--     versionIndex versionArray index = fromMaybe (-1) $ INT.fromString =<< versionArray !! index

iosVersionSplit :: String -> Version
iosVersionSplit versionName =
  let versionArray = (split (Pattern ".") versionName)
      major = versionIndex versionArray 0
      minor = versionIndex versionArray 1
      maintenance = versionIndex versionArray 2
  in Version{major, minor, maintenance}
  where
    versionIndex :: Array String -> Int -> Int
    versionIndex versionArray index = fromMaybe (-1) $ INT.fromString =<< versionArray !! index

appUpdatedFlow :: FCMBundleUpdate -> FlowBT String Unit
appUpdatedFlow payload = do
  -- modifyScreenState $ AppUpdatePopUpScreenType (\appUpdatePopUpScreenState → appUpdatePopUpScreenState {updatePopup = AppUpdated ,appUpdatedView{secondaryText=payload.description,primaryText=payload.title,coverImageUrl=payload.image}})
  UI.showPopUpOverlay restartFlow
  where 
    restartFlow = do
      liftFlowBT showSplash
      liftFlowBT reboot

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
-- getIosVersion :: (Version String) -> IosVersion
-- getIosVersion version =
--   let iosVersion = iosVersionSplit $ version.latestVersion
--   where 
--     mkIOSVersion :: Int -> Int -> Int -> Boolean -> IosVersion
--     mkIOSVersion majorUpdateIndex minorUpdateIndex patchUpdateIndex enableForceUpdateIOS =
--       { majorUpdateIndex, minorUpdateIndex, patchUpdateIndex, enableForceUpdateIOS }

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