module DecodeUtil where

import Prelude
import Data.Function.Uncurried
import Data.Maybe (Maybe(..), maybe)
import Foreign.Generic (class Decode, Foreign, decode, ForeignError(..))
import Control.Monad.Except (runExcept)
import Data.Either (Either(..), hush)
import Debug
import Foreign.Index (readProp)
import Record.Unsafe
import Data.List
import Data.List.NonEmpty (toList)
import Foreign (unsafeToForeign)
import Data.Maybe

foreign import getFromWindow :: Fn3 String (Maybe Foreign) (Foreign -> (Maybe Foreign)) (Maybe Foreign)

foreign import getAnyFromWindow :: forall a. Fn3 String (Maybe a) (a -> (Maybe a)) (Maybe a)

foreign import getFromWindowString :: Fn3 String (Maybe String) (String -> (Maybe String)) (Maybe String)

foreign import setInWindow :: Fn2 String String String

foreign import setAnyInWindow :: forall a. Fn2 String a a

foreign import unsafeSetForeign :: forall a. Fn3 String Foreign a Foreign

-- JSON Utils
foreign import parseJSON :: forall a. a -> Foreign

foreign import stringifyJSON :: forall a. Fn1 a String

foreign import toastWithLog :: String -> Unit

foreign import removeFromWindow :: String -> Unit

decodeForeignAny :: forall a. Decode a => Foreign -> a -> a
decodeForeignAny object defaultObject = maybe (defaultObject) identity $ decodeForeignAnyImpl object

decodeForeignAnyImpl :: forall a. Decode a => Foreign -> Maybe a
decodeForeignAnyImpl = hush <<< runExcept <<< decode

decodeForeignObject :: forall a. Decode (Record a) => Foreign -> (Record a) -> (Record a)
decodeForeignObject object defaultObject = maybe (defaultObject) identity $ decodeForeignObjImpl object defaultObject

decodeForeignObjImpl :: forall a. Decode (Record a) => Foreign -> (Record a) -> Maybe (Record a)
decodeForeignObjImpl object defaultObject = case runExcept $ decode object of
  Right decodedObj -> Just decodedObj
  Left err -> case (head $ toList err) of
    Nothing -> Just defaultObject
    Just item -> decodeForeignObjImpl (handleForeignError item object defaultObject Nothing) defaultObject

handleForeignError :: forall a b. Decode (Record a) => ForeignError -> Foreign -> (Record a) -> Maybe String -> Foreign
handleForeignError error object defaultObject mbKey = case error of
  ForeignError error -> do
    let _ = toastWithLog $ "Config Decode Failed - " <> error
    case mbKey of
      Just key -> updateTheKey key
      Nothing -> unsafeToForeign $ defaultObject
  TypeMismatch expected found ->
    let _ = toastWithLog $ "Config Decode Failed - " <> ("TypeMismatch for Key " <> (fromMaybe "" mbKey) <> " expected -> " <> expected <> " found " <> found)
    in case mbKey of
      Just key -> updateTheKey key
      Nothing -> unsafeToForeign $ defaultObject
  ErrorAtIndex index fnError ->
    let _ = toastWithLog $ "Config Decode Failed - " <> ("Error for Key " <> (fromMaybe "" mbKey) <> " ErrorAtIndex " <> (show index))
    in handleForeignError fnError object defaultObject mbKey
  ErrorAtProperty key fnError ->
    let _ = toastWithLog $ "Config Decode Failed - " <> ("Error for Key " <> "ErrorAtProperty " <> key <> " -> fnError " <> show fnError)
    in updateTheKey key
  where
  updateTheKey key =
    let isKeyExistInDefault = unsafeHas key defaultObject
    in if isKeyExistInDefault then do
          let
            value = unsafeGet key defaultObject
          runFn3 unsafeSetForeign key object value
        else
          unsafeToForeign $ defaultObject
