module DecodeUtil where

import Prelude (show, (<<<), ($))
import Data.Function.Uncurried (Fn1, Fn3)
import Data.Maybe (Maybe, maybe)
import Foreign.Generic (class Decode, Foreign, decode)
import Control.Monad.Except (runExcept)
import Data.Either (Either(..), hush)


foreign import getFromWindow :: Fn3 String (Maybe Foreign) (Foreign -> (Maybe Foreign)) (Maybe Foreign) 
-- JSON Utils
foreign import parseJSON :: String -> Foreign
foreign import stringifyJSON :: forall a. Fn1 a String

decodeForeignObject :: forall a. Decode a => Foreign -> a -> a
decodeForeignObject object defaultObject = maybe (defaultObject) (\object -> object) $ decodeForeignObjImpl object

decodeForeignObjImpl :: forall a. Decode a => Foreign -> Maybe a
decodeForeignObjImpl = hush <<< runExcept <<< decode
      