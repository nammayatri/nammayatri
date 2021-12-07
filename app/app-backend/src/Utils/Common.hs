{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Utils.Common
  ( module Utils.Common,
    module CoreCommon,
  )
where

import Beckn.Product.Validation.Context
import Beckn.Types.Common
import qualified Beckn.Types.Core.Cabs.Common.Context as Cab
import Beckn.Utils.Common as CoreCommon
import Data.Text as DT

validateContext :: (HasFlowEnv m r ["coreVersion" ::: Text, "domainVersion" ::: Text]) => Cab.Context -> m ()
validateContext context = do
  validateDomain Cab.MOBILITY context
  validateContextCommons context
