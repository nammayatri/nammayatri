module Domain.Action.Auth (auth) where

import Beckn.InternalAPI.Auth.API
import Beckn.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Beckn.Storage.Hedis (HedisFlow)
import Beckn.Utils.Common
import EulerHS.Prelude
import Tools.Auth

auth :: (EsqDBReplicaFlow m r, HedisFlow m r, HasField "authTokenCacheExpiry" r Seconds) => Token -> m PersonId
auth token = do
  verifyPerson token <&> (.getId)
