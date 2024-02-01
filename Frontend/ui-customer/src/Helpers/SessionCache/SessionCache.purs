module SessionCache (module SessionCache , module ReExport)where

import Prelude
import DecodeUtil (decodeForeignObject, getAnyFromWindow, parseJSON, setAnyInWindow, stringifyJSON)
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
          decodeObject = decodeForeignObject (parseJSON objString) []
      in runFn2 setAnyInWindow key decodeObject
    Just value -> value

removeSOSAlarmStatus :: String -> SosAlarmStatus
removeSOSAlarmStatus id = setSosAlarmStatus $ delete id $ getSosAlarmStatus ReExport.sosAlarmStatus


setSosAlarmStatus :: SosAlarmStatus -> SosAlarmStatus
setSosAlarmStatus obj = 
  let stringifiedObj = stringifyJSON obj
      _ =  runFn2 setKeyInSharedPref "SOS_ALARM_STATUS" stringifiedObj
  in runFn2 setAnyInWindow ReExport.sosAlarmStatus obj
