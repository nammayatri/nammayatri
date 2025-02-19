module Helpers.BBPSUtils where

import Common.Resources.Constants as Constant
import Common.Styles.Colors as Color
import Common.Types.App
import Constants.Configs (getPolylineAnimationConfig)
import Control.Monad.Except (runExcept)
import Data.Either (Either(..), hush)
import Data.Function.Uncurried (Fn2(..), Fn3, runFn3, Fn1, Fn4, runFn2, Fn5)
import Data.Generic.Rep (class Generic)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Aff (makeAff, nonCanceler, Fiber, Aff, launchAff)
import PrestoDOM.Core (isScreenActive)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Effect.Class (liftEffect)
import Effect.Uncurried
import Engineering.Helpers.Commons (screenHeight, screenWidth, os, callbackMapper, parseFloat, liftFlow)
import Foreign (Foreign)
import Foreign.Class (class Decode, class Encode, encode)
import Foreign.Generic (decodeJSON)
import Language.Types (STR(..))
import Prelude
import Presto.Core.Flow (Flow, doAff)
import Presto.Core.Types.Language.Flow
import Presto.Core.Utils.Encoding (defaultDecode, defaultEncode)
import Data.Eq.Generic (genericEq)
import Data.Array (head, (!!))

-- 
type BBPSSdkPayload =
  { action :: String
  , mobileNumber :: String
  }

makeBBPSSdkPayload :: String -> String -> BBPSSdkPayload
makeBBPSSdkPayload action mobileNumber =
  { action: action
  , mobileNumber: mobileNumber
  }

-- foreign import startBBPSMicroApp :: forall action. EffectFn2 String String Unit