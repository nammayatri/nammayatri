module Storage.Queries.Dunzo where

import Beckn.Storage.Redis.Queries
import Beckn.Types.Common
import EulerHS.Prelude
import Types.Common as T

insertToken :: MonadFlow m => T.ClientId -> Token -> m ()
insertToken clientId token =
  setExRedis ("Dunzo_token_" <> getClientId clientId) (T.getToken token) 3600 -- 1 hr

getToken :: MonadFlow m => T.ClientId -> m (Maybe Token)
getToken clientId = do
  mtext <- getKeyRedis ("Dunzo_token_" <> getClientId clientId)
  return $ Token <$> mtext
