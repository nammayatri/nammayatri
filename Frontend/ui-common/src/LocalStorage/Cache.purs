module LocalStorage.Cache where


-- import JBridge (setKeyInSharedPref)
import Data.Maybe (Maybe(..))
import Prelude
import Data.Function.Uncurried (Fn2(..), Fn3(..), runFn2, runFn3)
import DecodeUtil

foreign import getFromCache :: forall a.Fn3 String (Maybe a) (a -> (Maybe a)) (Maybe a)
foreign import setInCache ::forall a. Fn2 String a a
foreign import clearCache :: String -> Unit
foreign import setKeyInSharedPref :: Fn2 String String Unit


getValueFromCache :: forall a. String -> (String -> a) -> a
getValueFromCache key getIfNothing = do
  let mbValue = runFn3 getFromCache key Nothing Just
  case mbValue of
    Nothing -> 
      let value = getIfNothing key
      in runFn2 setInCache key value
    Just value -> value

setValueToCache :: forall a. String -> a -> (a -> String) -> a
setValueToCache key value (getValue) = 
  let _ =  runFn2 setKeyInSharedPref key $ getValue value
  in runFn2 setInCache key value

removeValueFromCache :: String -> Unit
removeValueFromCache key = clearCache key