module Utils.Hedis (module Utils.Hedis, module Reexport) where

import Beckn.Prelude
import Beckn.Storage.Hedis as Reexport
import Beckn.Storage.Hedis.AppPrefixes as Reexport

clearList :: (HedisFlow m env) => Text -> m ()
clearList key = lTrim key 2 1
