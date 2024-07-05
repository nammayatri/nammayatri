module Helpers.Pooling where

import Prelude (Unit, bind, ($), ($>), (&&), (<<<), (==))
import Data.Either (Either(..))
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (makeAff, nonCanceler)
import Effect.Uncurried (EffectFn3, runEffectFn3)
import Engineering.Helpers.Commons (getRandomID, liftFlow, os)
import Presto.Core.Types.Language.Flow as PrestoFlow

foreign import startTimerForDelay :: EffectFn3 String Number (Unit -> Effect Unit) Unit

foreign import isTimerExist :: Effect Boolean

delay :: forall st. Milliseconds -> PrestoFlow.Flow st Unit
delay time = do
  timer <- liftFlow $ isTimerExist
  if os == "IOS" && timer then
    delayViaTimer time
  else
    PrestoFlow.delay time

delayViaTimer :: forall st. Milliseconds -> PrestoFlow.Flow st Unit
delayViaTimer (Milliseconds time) =
  let id = getRandomID 100000
  in PrestoFlow.doAff $ makeAff \cb -> runEffectFn3 startTimerForDelay id time (cb <<< Right) $> nonCanceler
