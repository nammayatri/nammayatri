module Domain.Action.Auth (auth) where

import EulerHS.Prelude
import Kernel.InternalAPI.Auth.API
import Kernel.Storage.Hedis (HedisFlow)
import Kernel.Utils.Common
import Tools.Auth

auth :: (EsqDBFlow m r, HedisFlow m r, HasField "authTokenCacheExpiry" r Seconds) => Token -> m PersonId
auth token = do
  verifyPerson token <&> (.getId)
