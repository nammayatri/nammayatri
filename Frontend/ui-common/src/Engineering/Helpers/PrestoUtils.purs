module Helpers.PrestoUtils where

import Prelude
import DecodeUtil
import Data.Maybe
import Control.Monad.Trans.Class (lift)
import Types.App(FlowBT)
import Common.Types.App
import Engineering.Helpers.Accessor
import Data.Lens ((^.))
import PrestoDOM 
import Engineering.Helpers.BackTrack (liftFlowBT)
import Data.Function.Uncurried
import Engineering.Helpers.Commons as EHC
import Presto.Core.Types.Language.Flow (fork)

initUIWrapper :: String -> FlowBT String Unit
initUIWrapper _ = do 
  let globalPayload =  EHC.getGlobalPayload "__payload"
  case globalPayload of
    Nothing -> lift $ lift $ initUI
    Just payload -> liftFlowBT $ initUIWithNameSpace "default" ((payload ^. _payload) ^. _fragmentViewGroups >>= (\a -> a ^. _main))

getFragmentView :: String -> Maybe String 
getFragmentView _ = do
   let globalPayload =  EHC.getGlobalPayload "__payload"
   case globalPayload of
    Nothing -> Nothing
    Just payload -> ((payload ^. _payload) ^. _fragmentViewGroups >>= (\a -> a ^. _main))