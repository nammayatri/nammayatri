module Engineering.Helpers.SQLiteUtils where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Function.Uncurried (Fn1, Fn2, Fn3, Fn4, Fn5, Fn6) 
import Engineering.Helpers.SQLiteUtils.Schema

foreign import deleteDb :: Fn1 String Unit
foreign import createTable :: Fn3 String String SqlSchema Unit
foreign import deleteTable :: Fn2 String String Unit
foreign import addToSqlite :: forall st. Fn3 String String st Unit
foreign import readFromSqlite :: forall st. Fn6 String String String (Array String) (st -> Maybe st) (Maybe st) (Maybe st)
foreign import deleteFromSqlite :: Fn4 String String String (Array String) Boolean
foreign import updateInSqlite :: forall st. Fn5 String String String (Array String) st Int

dbName :: String
dbName = "atlas-app"

driverTableName :: String
driverTableName = "DriverInfo"

