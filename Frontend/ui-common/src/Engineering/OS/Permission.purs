{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Engineering.OS.Permission where

import Prelude

import Control.Monad.Except (runExcept, throwError)
import Control.Monad.Rec.Class (class MonadRec, Step(Done, Loop), tailRecM)
import Data.Array (singleton, zip)
import Data.Either (Either(..))
import Data.Foldable (all)
import Data.Function.Uncurried (Fn3, runFn3)
import Data.List (List(..), fromFoldable, (:))
import Data.String (Pattern(..), contains)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (makeAff, Aff, nonCanceler, Canceler)
import Effect.Class (liftEffect)
import Effect.Exception (Error, error)
import Foreign.Generic (decodeJSON)
import Presto.Core.Types.Language.Flow (Flow, checkPermissions, takePermissions)
import Presto.Core.Types.Permission (Permission(..), PermissionResponse, PermissionStatus(..))
import Types.App (GlobalState)

foreign import getPermissionStatus' :: Array String -> (Effect String)
foreign import requestPermission' :: Fn3 (Error -> Effect Unit) (String -> Effect Unit) String (Effect Unit)

toAndroidPermission :: Permission -> String
-- toAndroidPermission PermissionSendSms = "android.permission.READ_SMS"
-- toAndroidPermission PermissionReadPhoneState = "android.permission.READ_PHONE_STATE"
-- toAndroidPermission PermissionWriteStorage = "android.permission.WRITE_EXTERNAL_STORAGE"
-- toAndroidPermission PermissionReadStorage = "android.permission.READ_EXTERNAL_STORAGE"
toAndroidPermission _ = ""

allPermissionGranted :: Array PermissionResponse -> Boolean
allPermissionGranted = all (\(Tuple _ status) -> status == PermissionGranted)

getStoragePermission :: Flow GlobalState Boolean
getStoragePermission =
  ifM (storageGranted) (pure true) (askForStorage)
  where
    storageGranted :: Flow GlobalState Boolean
    storageGranted = do
       status <- checkPermissions [PermissionWriteStorage]
       case status of
        PermissionGranted -> pure true
        _ -> pure false
    askForStorage :: Flow GlobalState Boolean
    askForStorage = pure <<< allPermissionGranted =<< takePermissions [PermissionWriteStorage]

storagePermissionGranted :: Flow GlobalState Boolean
storagePermissionGranted = do
   status <- checkPermissions [PermissionWriteStorage]
   case status of
    PermissionGranted -> pure true
    _ -> pure false

getPermissionStatus :: Permission -> Aff Boolean
getPermissionStatus permission = do
  value <- liftEffect $ getPermissionStatus' $ singleton $ toAndroidPermission permission
  pure $ contains (Pattern "true") value

checkIfPermissionsGranted :: Array Permission -> Aff PermissionStatus
checkIfPermissionsGranted permissions = do
  check <- allM getPermissionStatus $ fromFoldable permissions
  pure $ if check
    then PermissionGranted
    else PermissionDeclined

_requestPermissions :: (Either Error String -> Effect Unit) -> String -> Effect Canceler
_requestPermissions cb str = do
  runFn3 requestPermission' (Left >>> cb) (Right >>> cb) str
  pure $ nonCanceler

requestPermissions :: Array Permission -> Aff (Array PermissionResponse)
requestPermissions permissions = do
  response <- makeAff (\cb -> _requestPermissions cb $ show jPermission)
  case runExcept $ decodeJSON response of
    Right (statuses :: Array Boolean) -> pure $ zip permissions (map toResponse statuses)
    Left err -> throwError (error (show err))
  where
    toResponse wasGranted = if wasGranted then PermissionGranted else PermissionDeclined
    jPermission = map toAndroidPermission permissions


allM :: forall m a. MonadRec m => (a -> m Boolean) -> List a -> m Boolean
allM p = tailRecM go where
  go Nil    = pure $ Done true
  go (x:xs) = ifM (p x) (pure $ Loop xs) (pure $ Done false)
