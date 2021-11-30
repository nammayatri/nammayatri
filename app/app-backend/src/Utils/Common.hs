{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Utils.Common
  ( module Utils.Common,
    module CoreCommon,
  )
where

import Beckn.Product.Validation.Context
import Beckn.Types.Common
import qualified Beckn.Types.Core.Migration1.Common.Context as Mig1
import Beckn.Utils.Common as CoreCommon
import Data.Text as DT

validateContextMig1 :: (HasFlowEnv m r ["coreVersion" ::: Text, "domainVersion" ::: Text]) => Mig1.Context -> m ()
validateContextMig1 context = do
  validateDomainMig1 Mig1.MOBILITY context
  validateContextCommonsMig1 context
