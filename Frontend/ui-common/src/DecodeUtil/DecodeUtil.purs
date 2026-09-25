module DecodeUtil where

import Prelude
import Data.Function.Uncurried
import Data.Maybe (Maybe(..), maybe)
import Foreign.Generic (class Decode, class Encode, Foreign, encode, decode, ForeignError(..))
import Control.Monad.Except (runExcept)
import Data.Either (Either(..), hush)
import Debug
import Foreign.Index (readProp)
import Record.Unsafe hiding (unsafeGet, unsafeHas)
import Data.List
import Data.List.NonEmpty (toList)
import Foreign (unsafeToForeign)
import Data.Maybe
import Effect (Effect)
import Effect.Aff (Fiber)

foreign import getFromWindow :: Fn3 String (Maybe Foreign) (Foreign -> (Maybe Foreign)) (Maybe Foreign)

foreign import getAnyFromWindow :: forall a. Fn3 String (Maybe a) (a -> (Maybe a)) (Maybe a)

foreign import storeFiber :: forall a. Fiber a -> Effect Unit

foreign import resetFibers :: Unit -> Effect Unit

foreign import getFibers :: forall a. String -> Effect (Array (Fiber a))

foreign import getFromWindowString :: Fn3 String (Maybe String) (String -> (Maybe String)) (Maybe String)

foreign import setInWindow :: Fn2 String String String

foreign import setAnyInWindow :: forall a. Fn2 String a a

foreign import unsafeSetForeign :: forall a. Fn3 String Foreign a Foreign

foreign import unsafeGet :: forall a. Fn2 String Foreign a
foreign import unsafeHas :: forall a. Fn2 String Foreign Boolean

-- JSON Utils
foreign import parseJSON :: forall a. a -> Foreign

foreign import stringifyJSON :: forall a. Fn1 a String

foreign import toastWithLog :: String -> Unit

foreign import removeFromWindow :: String -> Unit

decodeForeignAny :: forall a. Decode a => Foreign -> a -> a
decodeForeignAny object defaultObject = maybe (defaultObject) identity $ decodeForeignAnyImpl object

decodeForeignAnyImpl :: forall a. Decode a => Foreign -> Maybe a
decodeForeignAnyImpl = hush <<< runExcept <<< decode

decodeForeignObject :: forall a. Decode (Record a) => Encode (Record a) => Foreign -> (Record a) -> (Record a)
decodeForeignObject object defaultObject = maybe (defaultObject) identity $ decodeForeignObjImpl object defaultObject

decodeForeignObjImpl :: forall a. Decode (Record a) => Encode (Record a) => Foreign -> (Record a) -> Maybe (Record a)
decodeForeignObjImpl object defaultObject = case runExcept $ decode object of
  Right decodedObj -> Just decodedObj
  Left err -> case (head $ toList err) of
    Nothing -> Just defaultObject
    Just item -> decodeForeignObjImpl (handleForeignError item object (encode defaultObject) Nothing) defaultObject

handleForeignError :: ForeignError -> Foreign -> Foreign -> Maybe String -> Foreign
handleForeignError error object defaultObject mbKey = case error of
  ForeignError error -> do
    let _ = toastWithLog $ "Config Decode Failed - " <> error
    case mbKey of
      Just key -> updateTheKey key
      Nothing -> defaultObject
  TypeMismatch expected found ->
    let _ = toastWithLog $ "Config Decode Failed - " <> ("TypeMismatch for Key " <> (fromMaybe "" mbKey) <> " expected -> " <> expected <> " found " <> found)
    in case mbKey of
      Just key -> updateTheKey key
      Nothing -> defaultObject
  ErrorAtIndex index fnError ->
    let _ = toastWithLog $ "Config Decode Failed - " <> ("Error for Key " <> (fromMaybe "" mbKey) <> " ErrorAtIndex " <> (show index))
    in handleForeignError fnError object defaultObject mbKey
  ErrorAtProperty key fnError ->
    let _ = toastWithLog $ "Config Decode Failed - " <> ("Error for Key " <> "ErrorAtProperty " <> key <> " " <> (show fnError))
    in updateTheKey key
  where
  updateTheKey key =
    let isKeyExistInDefault = runFn2 unsafeHas key defaultObject
    in if isKeyExistInDefault then do
          let
            value = runFn2 unsafeGet key defaultObject
          runFn3 unsafeSetForeign key object value
        else defaultObject


setKeyInWindow :: forall a. String -> a -> Unit
setKeyInWindow key val = 
  let
    _ =  runFn2 setAnyInWindow key val 
  in unit

getKeyFromWindow :: forall a. String -> Maybe a 
getKeyFromWindow key = runFn3 getAnyFromWindow key Nothing Just

getKeyWithDefaultFromWindow :: forall a. String -> a -> a 
getKeyWithDefaultFromWindow key defaultVal = 
  case getKeyFromWindow key of
    Just val -> val
    Nothing -> defaultVal
