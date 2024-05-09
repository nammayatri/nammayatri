module Engineering.Helpers.SQLiteUtils where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Function.Uncurried (Fn1, Fn2, Fn3, Fn4, Fn5, Fn6, runFn3) 
import Effect
import Effect.Class (liftEffect)
import Data.Traversable (traverse_)
import Foreign (Foreign)

foreign import deleteDb :: Fn1 String Unit
foreign import createTable :: Fn3 String String SqlSchema Unit
foreign import deleteTable :: Fn2 String String Unit
foreign import addToSqlite :: forall st. Fn3 String String st (Effect Unit)
foreign import readFromSqlite :: forall st. Fn6 String String String (Array String) (st -> Maybe st) (Maybe st) (Maybe st)
foreign import deleteFromSqlite :: Fn4 String String String (Array String) Boolean
foreign import updateInSqlite :: forall st. Fn5 String String String (Array String) st Int
foreign import executeQuery :: forall st. Fn4 String String (st -> Maybe st) (Maybe st) (Maybe st)
-- foreign import executeQuery :: Fn2 String String Foreign


addMultipleRowsToSQlite :: forall st. String -> String -> Array st -> Effect Unit
addMultipleRowsToSQlite dbName tableName rows = do
    traverse_ (runFn3 addToSqlite dbName tableName) rows

type ColumnSchema = { key :: String, "type" :: String }
type SqlSchema = Array ColumnSchema