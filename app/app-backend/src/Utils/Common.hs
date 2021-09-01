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
import Beckn.Types.Id
import Beckn.Utils.Common as CoreCommon
import Data.Text as DT
import qualified Data.Text as T
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified Test.RandomStrings as RS

generateShortId :: MonadFlow m => m (ShortId a)
generateShortId = ShortId . T.pack <$> L.runIO (RS.randomString (RS.onlyAlphaNum RS.randomASCII) 10)

validateContext :: (HasFlowEnv m r ["coreVersion" ::: Text, "domainVersion" ::: Text]) => Text -> Context -> m ()
validateContext action context = do
  validateDomain MOBILITY context
  validateContextCommons action context
