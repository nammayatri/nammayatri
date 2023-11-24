module DecodeUtil where

import Prelude (show, ($))
import Data.Function.Uncurried (Fn1, Fn3)
import Data.Maybe (Maybe)
import Foreign.Generic (class Decode, Foreign, decode)
import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Log (printLog)


foreign import getFromWindow :: Fn3 String (Maybe Foreign) (Foreign -> (Maybe Foreign)) (Maybe Foreign) 
-- JSON Utils
foreign import parseJSON :: String -> Foreign
foreign import stringifyJSON :: forall a. Fn1 a String

decodeForeignObject :: forall a. Decode a => Foreign -> a -> a
decodeForeignObject object defaultObject = case runExcept $ decode object of
  Right decodedObj -> decodedObj
  Left err -> do
    let _ = printLog "Not able to decode config not able to  find in default config" (show err)
    defaultObject
