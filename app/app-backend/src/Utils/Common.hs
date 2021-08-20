{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Utils.Common
  ( module Utils.Common,
    module CoreCommon,
  )
where

import Beckn.Product.Validation.Context
import Beckn.Types.Common
import Beckn.Types.Core.Context
import Beckn.Types.Core.Domain
import Beckn.Utils.Common as CoreCommon
import Data.Text as DT

validateContext :: (HasFlowEnv m r ["coreVersion" ::: Text, "domainVersion" ::: Text]) => Text -> Context -> m ()
validateContext action context = do
  validateDomain MOBILITY context
  validateContextCommons action context
