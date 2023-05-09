module Dynamic.Dynamic where

import Prelude
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Common.Types.App (GlobalPayload)
import Types.App (FlowBT)
import Presto.Core.Types.Language.Flow (doAff)
import Control.Monad.Except.Trans (lift)

-- foreign import dflow :: forall a. EffectFnAff a
-- foreign import dynamicFlow2 :: forall a. String -> EffectFnAff a

-- dynamicFlow :: String -> Aff String
-- dynamicFlow s = do
--     aff <- fromEffectFnAff dflow
--     aff s

-- dynamicBaseAppFlow :: GlobalPayload -> FlowBT String Unit
-- dynamicBaseAppFlow payload = do
--     func <- lift $ lift $ doAff $ fromEffectFnAff $ dynamicFlow2 "baseAppFlow"
--     func payload