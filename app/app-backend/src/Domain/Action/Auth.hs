module Domain.Action.Auth (auth) where

import Beckn.InternalAPI.Auth.API
import Beckn.Utils.Common
import EulerHS.Prelude
import Utils.Auth

auth :: (EsqDBFlow m r, HasField "authTokenCacheExpiry" r Seconds) => Token -> m PersonId
auth token = do
  verifyPerson token <&> (.getId)
