module DecodeUtil where

import Prelude (show, (<<<), ($))
import Data.Function.Uncurried
import Data.Maybe (Maybe, maybe)
import Foreign.Generic (class Decode, Foreign, decode)
import Control.Monad.Except (runExcept)
import Data.Either (Either(..), hush)


foreign import getFromWindow :: Fn3 String (Maybe Foreign) (Foreign -> (Maybe Foreign)) (Maybe Foreign)
foreign import getFromWindowString :: Fn3 String (Maybe String) (String -> (Maybe String)) (Maybe String)
foreign import setInWindow :: Fn2 String String String
-- JSON Utils
foreign import parseJSON :: String -> Foreign
foreign import stringifyJSON :: forall a. Fn1 a String

decodeForeignObject :: forall a. Decode a => Foreign -> a -> a
decodeForeignObject object defaultObject = maybe (defaultObject) (\object -> object) $ decodeForeignObjImpl object

decodeForeignObjImpl :: forall a. Decode a => Foreign -> Maybe a
decodeForeignObjImpl = hush <<< runExcept <<< decode
      