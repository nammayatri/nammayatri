{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Helpers.Commons where

import Prelude
import Control.Monad.Except (runExcept, throwError)
import Control.Monad.Rec.Class (class MonadRec, Step(Done, Loop), tailRecM)
import Control.Monad.State as S
import Data.Array (singleton, zip)
import Data.Either (Either(..), hush)
import Data.Foldable (foldl)
import Data.Function.Uncurried (Fn2, Fn3, runFn3)
import Data.Function.Uncurried (Fn3, runFn3)
import Data.Int (fromString, toNumber)
import Data.List (List(..), fromFoldable, (:))
import Data.Maybe (fromMaybe, Maybe(..))
import Data.Number (pow, round)
import Data.String (Pattern(..), contains)
import Data.String as DS
import Debug (spy)
import Effect (Effect)
import Effect.Aff (Aff, Canceler, attempt, error, makeAff, nonCanceler)
import Effect.Aff.AVar (new)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception (Error)
import Effect.Uncurried (EffectFn1, EffectFn3, EffectFn7, EffectFn8, mkEffectFn6, runEffectFn7, runEffectFn8)
import Foreign.Generic (Foreign, decode, decodeJSON)
import Foreign.Object (empty, insert, lookup)
import JSURI (decodeURIComponent)
import Presto.Core.Language.Runtime.API (APIRunner)
import Presto.Core.Language.Runtime.Interpreter (PermissionCheckRunner, PermissionRunner(..), PermissionTakeRunner, Runtime(..), run)
import Presto.Core.Types.API (Header(..), Headers(..), Request(..), RestAPIOptions(..)) as API
import Presto.Core.Types.API (standardEncodeJSON)
import Presto.Core.Types.Language.Flow (Flow, defaultState, doAff)
import Presto.Core.Types.Permission (Permission, PermissionResponse, PermissionStatus(..))

foreign import callAPI :: EffectFn7 String String String String Boolean Boolean String Unit

foreign import callAPIWithOptions :: EffectFn8 String String String String Boolean Boolean String String Unit

foreign import callbackMapper :: forall a. a -> String

foreign import atobImpl :: String -> String

foreign import getWindowVariable :: forall a. String -> (a -> (Maybe a)) -> (Maybe a) -> Effect a

foreign import screenWidth :: Unit -> Int

foreign import getDeviceHeight :: Unit -> Int

foreign import getScreenPpi :: Unit -> Int

foreign import screenHeight :: Unit -> Int

foreign import safeMarginTopImpl :: Unit -> Int

foreign import safeMarginBottomImpl :: Unit -> Int

foreign import getNewIDWithTag :: String -> String

foreign import getOs :: Unit -> String

foreign import getPermissionStatusImpl :: Array String -> (Effect String)

foreign import requestPermissionImpl :: Fn3 (Error -> Effect Unit) (String -> Effect Unit) String (Effect Unit)

foreign import setKeyInSharedPref :: Fn2 String String Unit

foreign import getKeyInSharedPrefKeys :: String -> String

foreign import waitTillDriverAppBoot :: forall a. EffectFn1 (a -> Effect Unit) Unit

foreign import openDriverApp :: EffectFn1 String Unit

foreign import bootDriverInParallel :: EffectFn1 String Unit

foreign import getRandomID :: Int -> String

foreign import emitEvent :: EffectFn3 String String Foreign Unit

os :: String
os = getOs unit

type AffError
  = (Error -> Effect Unit)

type AffSuccess s
  = (s -> Effect Unit)

liftFlow :: forall val st. (Effect val) -> Flow st val
liftFlow effVal = doAff do liftEffect (effVal)

flowRunner :: ∀ return st. st -> Flow st return -> Aff (Either Error return)
flowRunner state flow = do
  let
    freeFlow = S.evalStateT $ run standardRunTime flow
  attempt $ new (defaultState state) >>= freeFlow

permissionCheckRunner :: PermissionCheckRunner
permissionCheckRunner = checkIfPermissionsGranted

permissionTakeRunner :: PermissionTakeRunner
permissionTakeRunner = requestPermissions

permissionRunner :: PermissionRunner
permissionRunner = PermissionRunner permissionCheckRunner permissionTakeRunner

standardRunTime :: Runtime
standardRunTime =
  Runtime
    pure
    permissionRunner
    apiRunner

safeMarginTop :: Int
safeMarginTop = safeMarginTopImpl unit

safeMarginTopWithDefault :: Int -> Int
safeMarginTopWithDefault def =
  let
    safeMargin = safeMarginTop
  in
    if safeMargin == 0 then def else safeMargin

safeMarginBottomWithDefault :: Int -> Int
safeMarginBottomWithDefault def =
  let
    safeMargin = safeMarginBottom
  in
    if safeMargin == 0 then def else safeMargin

safeMarginBottom :: Int
safeMarginBottom = safeMarginBottomImpl unit

strToBool :: String -> Maybe Boolean
strToBool value = case (DS.toLower value) of
  "true" -> Just true
  "false" -> Just false
  _ -> Nothing

truncate :: Int -> Number -> Number
truncate precision num = (round (num * (10.0 `pow` (toNumber precision)))) / (10.0 `pow` (toNumber precision))

apiRunner :: APIRunner
apiRunner (API.Request request@{ headers: API.Headers hs }) =
  makeAff
    ( \cb -> do
        void $ pure $ spy "callAPI request" request
        case request.options of
          Nothing -> do
            _ <- runEffectFn7 callAPI (show request.method) request.url request.payload (standardEncodeJSON headers) shouldFormEncode isSSLPinnedURL $ callback cb
            pure $ nonCanceler
          Just (API.RestAPIOptions ops) -> do
            _ <- runEffectFn8 callAPIWithOptions (show request.method) request.url request.payload (standardEncodeJSON headers) shouldFormEncode isSSLPinnedURL (standardEncodeJSON ops) $ callback cb
            pure $ nonCanceler
    )
  where
  callback cb =
    callbackMapper
      $ mkEffectFn6
      $ \status response statusCode url responseHeaders urlEncodedResponse -> do
          let
            formattedResponse =
              { status: status
              , response: extractResponse response urlEncodedResponse
              , code: fromMaybe (-3) (fromString statusCode)
              , responseHeaders: fromMaybe empty $ hush $ runExcept $ decodeJSON $ atobImpl responseHeaders
              }
          let
            _ = spy "callAPI response " $ formattedResponse
          let
            _ = spy "callAPI url " url
          let
            _ = spy "response " $ formattedResponse.response
          cb $ Right formattedResponse

  headers = foldl (\acc (API.Header key value) -> insert key value acc) empty hs

  isSSLPinnedURL = lookup "x-pinned" headers == Just "true"

  shouldFormEncode = lookup "Content-Type" headers == Just "application/x-www-form-urlencoded"

  extractResponse response urlEncodedResponse = do
    case hush $ runExcept $ decode urlEncodedResponse of
      Just resp -> fromMaybe (atobImpl response) $ decodeURIComponent resp
      _ -> atobImpl response

getPermissionStatus :: Permission -> Aff Boolean
getPermissionStatus _ = do
  value <- liftEffect $ getPermissionStatusImpl $ singleton $ ""
  pure $ contains (Pattern "true") value

checkIfPermissionsGranted :: Array Permission -> Aff PermissionStatus
checkIfPermissionsGranted permissions = do
  check <- allM getPermissionStatus $ fromFoldable permissions
  pure
    $ if check then
        PermissionGranted
      else
        PermissionDeclined

_requestPermissions :: (Either Error String -> Effect Unit) -> String -> Effect Canceler
_requestPermissions cb str = do
  runFn3 requestPermissionImpl (Left >>> cb) (Right >>> cb) str
  pure $ nonCanceler

requestPermissions :: Array Permission -> Aff (Array PermissionResponse)
requestPermissions permissions = do
  response <- makeAff (\cb -> _requestPermissions cb $ show "")
  case runExcept $ decodeJSON response of
    Right (statuses :: Array Boolean) -> pure $ zip permissions (map toResponse statuses)
    Left err -> throwError (error (show err))
  where
  toResponse wasGranted = if wasGranted then PermissionGranted else PermissionDeclined

allM :: forall m a. MonadRec m => (a -> m Boolean) -> List a -> m Boolean
allM p = tailRecM go
  where
  go Nil = pure $ Done true

  go (x : xs) = ifM (p x) (pure $ Loop xs) (pure $ Done false)

getHeightFromPercent :: Int -> Int
getHeightFromPercent percent =
  let
    scrHeight = (screenHeight unit)
  in
    ((scrHeight / 100) * percent)

-- To get Array of given length
getArray :: Int -> Array Int
getArray count = if count == 0 then [ count ] else [ count ] <> (getArray (count - 1))
