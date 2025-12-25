module SessionCache (module SessionCache , module ReExport)where

import Prelude
import DecodeUtil (decodeForeignAny, getAnyFromWindow, parseJSON, setAnyInWindow, stringifyJSON, getFromWindowString)
import Data.Maybe (Maybe(..))
import JBridge (getKeyInSharedPrefKeys, setKeyInSharedPref)
import Data.Function.Uncurried (runFn2, runFn3)
import Constants (appConfig, configuration_file, decodeAppConfig, defaultDensity, defaultSeparatorCount, dotJS, dotJSA, dotJSON, getSeparatorFactor, globalPayload, languageKey, sosAlarmStatus) as ReExport
import Data.Array (delete)
import SessionCache.Types (SosAlarmStatus)

getSosAlarmStatus :: String -> SosAlarmStatus
getSosAlarmStatus key = do
  let mbValue = runFn3 getAnyFromWindow key Nothing Just
  case mbValue of
    Nothing -> 
      let objString = getKeyInSharedPrefKeys "SOS_ALARM_STATUS"
          decodeObject = decodeForeignAny (parseJSON objString) []
      in runFn2 setAnyInWindow key decodeObject
    Just value -> value

removeSOSAlarmStatus :: String -> SosAlarmStatus
removeSOSAlarmStatus id = setSosAlarmStatus $ delete id $ getSosAlarmStatus ReExport.sosAlarmStatus


setSosAlarmStatus :: SosAlarmStatus -> SosAlarmStatus
setSosAlarmStatus obj = 
  let stringifiedObj = stringifyJSON obj
      _ =  runFn2 setKeyInSharedPref "SOS_ALARM_STATUS" stringifiedObj
  in runFn2 setAnyInWindow ReExport.sosAlarmStatus obj

getValueFromWindow :: String -> String
getValueFromWindow key = do
  let mbValue = runFn3 getFromWindowString key Nothing Just
  case mbValue of
    Nothing -> 
      let value = getKeyInSharedPrefKeys key
      in runFn2 setAnyInWindow key value
    Just value -> value


setValueInWindow :: String -> String -> String
setValueInWindow key value = 
  let _ =  runFn2 setKeyInSharedPref key value
  in runFn2 setAnyInWindow key value