module SessionCache (module SessionCache , module ReExport)where

import Prelude
import DecodeUtil
import Data.Maybe
import JBridge
import Data.Function.Uncurried
import Constants as ReExport
import Data.Array
import SessionCache.Types

getSosAlarmStatus :: String -> SosAlarmStatus
getSosAlarmStatus key = do
  let mbValue = runFn3 getAnyFromWindow key Nothing Just
  case mbValue of
    Nothing -> 
      let objString = getKeyInSharedPrefKeys "SOS_ALARM_STATUS"
          decodeObject = decodeForeignObject (parseJSON objString) []
      in runFn2 setAnyInWindow key decodeObject
    Just value -> value

setSosAlarmStatus :: SosAlarmStatus -> SosAlarmStatus
setSosAlarmStatus obj = 
  let stringifiedObj = stringifyJSON obj
      _ =  runFn2 setKeyInSharedPref "SOS_ALARM_STATUS" stringifiedObj
  in runFn2 setAnyInWindow ReExport.sosAlarmStatus obj


getCustomerId :: String -> String
getCustomerId key = do
  let mbValue = runFn3 getFromWindowString key Nothing Just
  case mbValue of
    Nothing ->
      let personId = getKeyInSharedPrefKeys "CUSTOMER_ID"
      in runFn2 setInWindow key personId
    Just value -> value
