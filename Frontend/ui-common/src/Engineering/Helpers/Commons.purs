{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Engineering.Helpers.Commons where

import Prelude

import Control.Monad.Except (runExcept)
import Control.Monad.Except.Trans (lift)
import Control.Monad.State as S
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn2)
import Data.Maybe (fromMaybe, Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, makeAff, nonCanceler, try)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Effect.Class (liftEffect)
import Effect.Exception (Error)
import Effect.Ref (Ref, read, write)
import Engineering.OS.Permission (checkIfPermissionsGranted, requestPermissions)
import Common.Types.Sdk (SDKRequest(..), SDKResponse(..))
import Foreign.Class (class Decode, class Encode)
import Presto.Core.Language.Runtime.API (APIRunner)
import Presto.Core.Language.Runtime.Interpreter (PermissionCheckRunner, PermissionRunner(..), PermissionTakeRunner, Runtime(..), run)
import Presto.Core.Types.API (Header(..), Headers(..), Request(..), URL,Response)
import Presto.Core.Types.Language.Flow (Flow, doAff,defaultState)
import Presto.Core.Utils.Encoding (defaultDecodeJSON, defaultEncodeJSON)
import Types.App (FlowBT, GlobalState, defaultGlobalState)
import Effect.Aff.AVar (new)
import Data.String(toLower)


foreign import showUI' :: Fn2 (String -> Effect  Unit) String (Effect Unit)
foreign import callAPI' :: AffError -> AffSuccess (Response String) -> NativeRequest -> (Effect Unit)
foreign import window' :: forall a. String -> (a -> (Maybe a)) -> (Maybe a) -> Effect a
foreign import setWindowVariable' :: forall a. String -> a -> Effect Unit
foreign import setScreen' :: String -> Effect Unit
foreign import screenWidth :: Unit -> Int
foreign import screenHeight :: Unit -> Int
foreign import bundleVersion :: Unit -> String
foreign import callSahay ::  String  -> EffectFnAff String
foreign import safeMarginTop' :: Unit -> Int
foreign import safeMarginBottom' :: Unit -> Int
foreign import getNewIDWithTag :: String -> String
foreign import getOs :: Unit -> String
foreign import setText' :: String -> String -> Effect Unit
foreign import countDown :: forall action. Int -> String -> (action -> Effect Unit) -> (Int -> String -> String -> String-> action)  -> Effect Unit
foreign import clearTimer :: String -> Unit
foreign import getExpiryTime :: String -> Boolean -> Int

os :: String
os = getOs unit

sendSdkRequest :: forall a b. Encode a => Decode b =>  SDKRequest a  -> Flow GlobalState (SDKResponse b)
sendSdkRequest request@(SDKRequest req) = do
  result <- doAff do (fromEffectFnAff <<< callSahay $ request')
  {--_ <- pure $ spy "Got result fromm makeAff"--}
  case runExcept $ defaultDecodeJSON result of
    Right response -> pure response
    Left err -> do
     {--_ <- pure $ spy "Failed to decode response : " (show err)--}
     pure $ SDKResponse
          {
            success : false,
            event : req.event,
            payload: Nothing
          }
  where
      request' = defaultEncodeJSON request

type NativeHeader = { field :: String , value :: String}
type NativeHeaders = Array NativeHeader
type AffError = (Error -> Effect Unit)
type AffSuccess s = (s -> Effect Unit)

newtype NativeRequest = NativeRequest
  { method :: String
  , url :: URL
  , payload :: String
  , headers :: NativeHeaders
  }


mkNativeRequest :: Request -> NativeRequest
mkNativeRequest (Request request@{headers: Headers hs}) = NativeRequest
                                          { method : show request.method
                                            , url: request.url
                                            , payload: request.payload
                                            , headers: mkNativeHeader <$> hs
                                            }

mkNativeHeader :: Header -> NativeHeader
mkNativeHeader (Header field val) = { field: field, value: val}

trackerIcon :: String -> String
trackerIcon vehicleType = case vehicleType of
                            "SEDAN" -> "tracker_sedan"
                            "SUV" -> "tracker_suv"
                            "HATCHBACK" -> "tracker_hatchback"
                            _ -> "map_car"

liftFlow :: forall val . (Effect val)  -> Flow GlobalState val
liftFlow effVal = doAff do liftEffect (effVal)

fromNull :: forall a. a -> Maybe a -> a
fromNull a b = fromMaybe a b

window :: forall a. String -> Flow GlobalState (Maybe a)
window key = liftFlow (window' key Just Nothing)

setWindowVariable :: forall a. String -> a -> Flow GlobalState Unit
setWindowVariable key value = liftFlow (setWindowVariable' key value)

flowRunner :: forall a. (Flow GlobalState a) -> Aff (Either Error a)
flowRunner flow = do
  let runtime  = Runtime pure permissionRunner apiRunner
  let freeFlow = S.evalStateT (run runtime flow)
  try $ new (defaultState defaultGlobalState) >>= freeFlow

permissionCheckRunner :: PermissionCheckRunner
permissionCheckRunner = checkIfPermissionsGranted

permissionTakeRunner :: PermissionTakeRunner
permissionTakeRunner = requestPermissions

permissionRunner :: PermissionRunner
permissionRunner = PermissionRunner permissionCheckRunner permissionTakeRunner

apiRunner :: APIRunner
apiRunner request = makeAff (\cb -> do
    _ <- callAPI' (cb <<< Left) (cb <<< Right) (mkNativeRequest request)
    pure $ nonCanceler)

readFromRef :: Ref GlobalState → FlowBT String GlobalState
readFromRef ref = lift $ lift $ doAff $ liftEffect $ read ref

writeToRef :: GlobalState → Ref GlobalState → FlowBT String Unit
writeToRef d ref = lift $ lift $ doAff $ liftEffect $ write d ref

modifyEpassRef :: (GlobalState → GlobalState) → Ref GlobalState → FlowBT String GlobalState
modifyEpassRef f ref = do
  d ← readFromRef ref
  writeToRef (f d) ref
  pure d

safeMarginTop :: Int
safeMarginTop = safeMarginTop' unit

safeMarginBottom :: Int
safeMarginBottom = safeMarginBottom' unit

strToBool :: String -> Maybe Boolean 
strToBool value = case (toLower value) of 
                    "true"  -> Just true 
                    "false" -> Just false
                    _       -> Nothing